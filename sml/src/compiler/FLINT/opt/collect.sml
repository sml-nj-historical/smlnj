(* copyright 1998 YALE FLINT PROJECT *)
(* monnier@cs.yale.edu *)

signature COLLECT =
sig
    
    (* Collect information about variables and function uses.
     * The info is accumulated in the map `m' *)
    val collect : FLINT.fundec -> unit

    (* query functions *)
    val recursive : FLINT.lvar -> bool
    val escaping  : FLINT.lvar -> bool	(* non-call uses *)
    val usenb     : FLINT.lvar -> int	(* nb of non-recursive uses *)
    (* val callnb    : FLINT.lvar -> int *)

    (* inc the "true=call,false=use" count *)
    val use    : bool -> FLINT.lvar -> unit
    (* dec the "true=call,false=use" count and call the function if zero *)
    val unuse  : (FLINT.lvar -> unit) -> bool -> FLINT.lvar -> unit
    (* transfer the counts of var1 to var2 *)
    val transfer : FLINT.lvar * FLINT.lvar -> unit
    (* add the counts of var1 to var2 *)
    val addto  : FLINT.lvar * FLINT.lvar -> unit
    (* delete the last reference to a variable *)
    val kill   : FLINT.lvar -> unit
    (* create a new var entry (true=fun, false=other) initialized to zero *)
    val new    : bool -> FLINT.lvar -> unit

    (* when creating a new var.  Used when alpha-renaming *)
    (* val copy   : FLINT.lvar * FLINT.lvar -> unit *)

    (* fix up function to keep counts up-to-date when getting rid of code.
     * the arg is only called for *free* variables becoming dead.
     * the first function returned just unuses an exp, while the
     * second unuses a function declaration (f,args,body) *)
    val unuselexp : (FLINT.lvar -> unit) -> 
	((FLINT.lexp -> unit) *
         ((FLINT.lvar * FLINT.lvar list * FLINT.lexp) -> unit))
    (* function to collect info about a newly created lexp *)
    val uselexp : FLINT.lexp -> unit

    (* This allows to execute some code and have all the resulting
     * changes made to the internal (for recursion) counters instead
     * of the external ones. For instance:
     *     inside f (fn () => call ~1 f)
     * would decrement the count of recursive function calls of f *)
    val inside : FLINT.lvar -> (unit -> 'a) -> 'a

    (* mostly useful for PPFlint *)
    val LVarString : FLINT.lvar -> string
end

structure Collect :> COLLECT =
struct
local
    structure F  = FLINT
    structure M  = Intmap
    structure LV = LambdaVar
    structure PP = PPFlint
in

val say = Control.Print.say
fun bug msg = ErrorMsg.impossible ("Collect: "^msg)
fun buglexp (msg,le) = (say "\n"; PP.printLexp le; say " "; bug msg)
fun bugval (msg,v) = (say "\n"; PP.printSval v; say " "; bug msg)
fun ASSERT (true,_) = ()
  | ASSERT (FALSE,msg) = bug ("assertion "^msg^" failed")

datatype info
  (* for functions we keep track of calls and escaping uses
   * and separately for external and internal (recursive) references *)
  = Fun of {ecalls: int ref, euses: int ref,
	    inside: bool ref,
	    icalls: int ref, iuses: int ref}
  | Var of int ref	(* for other vars, a simple use count is kept *)
  | Transfer of FLINT.lvar	(* for vars who have been transfered *)
    
exception NotFound
	      
val m : info M.intmap = M.new(128, NotFound)

(* map related helper functions *)
fun get lv = (M.map m lv)
		 (* handle x as NotFound =>
		 (say "\nCollect:get unknown var ";
		  PP.printSval (F.VAR lv);
		  say ". Assuming dead...";
		  raise x;
		  Var (ref 0)) *)

fun new true lv = M.add m (lv, Fun{ecalls=ref 0, euses=ref 0,
				   inside=ref false,
				   icalls=ref 0, iuses=ref 0})
  | new false lv = M.add m (lv, Var(ref 0))

fun LVarString lv =
    (LV.lvarName lv)^
    ((case get lv of
	 Var uses => "{"^(Int.toString (!uses))^"}"
       | Fun {ecalls,euses,icalls,iuses,...} =>
	 concat
	     ["{(", Int.toString (!ecalls), ",", Int.toString (!euses),
	      "),(", Int.toString (!icalls), ",", Int.toString (!iuses), ")}"]
       | Transfer _ => "{-}")
	 handle NotFound => "{?}")

(* adds the counts of lv1 to those of lv2 *)
fun addto (lv1,lv2) =
    let val info2 = get lv2
	val info1 = get lv1
    in case info1
	of Var uses1 =>
	   (case info2
	     of Var uses2 => uses2 := !uses2 + !uses1
	      | Fun {euses=eu2,inside=i2,iuses=iu2,...} =>
		if !i2 then iu2 := !iu2 + !uses1
		else eu2 := !eu2 + !uses1
	      | Transfer _ => bugval("transfering to a Transfer", F.VAR lv2))
	 | Fun {inside=i1,euses=eu1,iuses=iu1,ecalls=ec1,icalls=ic1,...} =>
	   (ASSERT(!iu1 + !ic1 = 0 andalso not(!i1), "improper fun transfer");
	    case info2
	     of Fun {inside=i2,euses=eu2,iuses=iu2,ecalls=ec2,icalls=ic2,...} =>
		if !i2 then (iu2 := !iu2 + !eu1; ic2 := !ic2 + !ec1)
		else (eu2 := !eu2 + !eu1; ec2 := !ec2 + !ec1)
	      | Var uses => uses := !uses + !eu1
	      | Transfer _ => bugval("transfering to a Transfer", F.VAR lv2))
	 | Transfer _ => bugval("transfering from a Transfer", F.VAR lv1)
    end
fun transfer (lv1,lv2) =
    (addto(lv1, lv2);
     M.add m (lv1, Transfer lv2))	(* note the transfer *)
	       
fun inc ri = (ri := !ri + 1)
fun dec ri = (ri := !ri - 1)

fun use call lv =
    case get lv
     of Var uses => inc uses
      | (Fun {inside=ref true, iuses=uses,icalls=calls,...} |
	 Fun {inside=ref false,euses=uses,ecalls=calls,...} ) =>
	(if call then inc calls else (); inc uses)
      | Transfer lv => use call lv

fun unuse undertaker call lv =
    let fun check uses =
	    if !uses < 0 then
		bugval("decrementing too much", F.VAR lv)
	    else if !uses = 0 then
		undertaker lv
	    else ()
    in case get lv
	of Var uses => (dec uses; check uses)
	 | Fun {inside=ref false,euses=uses,ecalls=calls,...} =>
	   (dec uses; if call then dec calls else ASSERT(!uses >= !calls, "unknown sanity"); check uses)
	 | Fun {inside=ref true, iuses=uses,icalls=calls,...} =>
	   (dec uses; if call then dec calls else ASSERT(!uses >= !calls, "unknown rec-sanity"))
	 | Transfer lv => unuse undertaker call lv
    end

fun usenb lv     = case get lv of (Fun{euses=uses,...} | Var uses) => !uses
				| Transfer _ => 0
fun used lv      = usenb lv > 0
fun recursive lv = case get lv of (Fun{iuses=uses,...} | Var uses) => !uses > 0
				| Transfer lv => (say "\nCollect:recursive on transfer"; recursive lv)
(* fun callnb lv    = case get lv of Fun{ecalls,...} => !ecalls | Var us => !us *)
fun escaping lv =
    case get lv
     of Fun{iuses,euses,icalls,ecalls,...}
	=> !euses + !iuses > !ecalls + !icalls
      | Var us => !us > 0 (* arbitrary, but I opted for the "safe" choice *)
      | Transfer lv => (say "\nCollect escaping on transfer"; escaping lv)

(* census of the internal part *)    
fun inside f thunk =
    case get f
     of Fun{inside=inside as ref false,...} =>
	(inside := true; thunk() before inside := false)
      | Fun _ => (say "\nalready inside "; PP.printSval(F.VAR f); thunk())
      | _ => bugval("trying to get inside a non-function", F.VAR f)

(* Ideally, we should check that usenb = 1, but we may have been a bit
 * conservative when keeping the counts uptodate *)
fun kill lv = (ASSERT(usenb lv >= 1, concat ["usenb lv >= 1 ", !PP.LVarString lv]); M.rmv m lv)

fun census new use = let
    (* val use = if inc then use else unuse *)
    fun call lv = use true lv
    val use = fn F.VAR lv => use false lv | _ => ()
    val newv = new false
    val newf = new true
    fun id x = x

    fun impurePO po = true		(* if a PrimOP is pure or not *)

    (* here, the use resembles a call, but it's safer to consider it as a use *)
    fun cpo (NONE:F.dict option,po,lty,tycs) = ()
      | cpo (SOME{default,table},po,lty,tycs) =
	(use (F.VAR default); app (use o F.VAR o #2) table)
    fun cdcon (s,Access.EXN(Access.LVAR lv),lty) = use (F.VAR lv)
      | cdcon _ = ()

    (* the actual function:
     * `uvs' is an optional list of booleans representing which of 
     * the return values are actually used *)
    fun cexp uvs lexp =
	case lexp
	 of F.RET vs => app use vs
(* 	    (case uvs *)
(* 	      of SOME uvs => (* only count vals that are actually used *) *)
(* 		 app (fn(v,uv)=>if uv then use v else ()) (ListPair.zip(vs,uvs)) *)
(* 	       | NONE => app use vs) *)

	  | F.LET (lvs,le1,le2) =>
	    (app newv lvs; cexp uvs le2; cexp (SOME(map used lvs)) le1)

	  | F.FIX (fs,le) =>
	    let fun cfun ((_,f,args,body):F.fundec) = (* census of a fundec *)
		    (app (newv o #1) args; inside f (fn()=> cexp NONE body))
		fun cfix fs = let	(* census of a list of fundecs *)
		    val (ufs,nfs) = List.partition (used o #2) fs
		in if List.null ufs then ()
		   else (app cfun ufs; cfix nfs)
		end
	    in app (newf o #2) fs; cexp uvs le; cfix fs
	    end
	       
	  | F.APP (F.VAR f,vs) =>
	    (call f; app use vs)

	  | F.TFN ((tf,args,body),le) =>
	    (newf tf; cexp uvs le;
	     if used tf then inside tf (fn()=> cexp NONE body) else ())

	  | F.TAPP (F.VAR tf,tycs) => call tf

	  | F.SWITCH (v,cs,arms,def) =>
	    (use v; Option.map (cexp uvs) def;
	     (* here we don't absolutely have to keep track of vars bound within
	      * each arm since these vars can't be eliminated anyway *)
	     app (fn (F.DATAcon(dc,_,lv),le) => (cdcon dc; newv lv; cexp uvs le)
		   | (_,le) => cexp uvs le)
		 arms)
		
	  | F.CON (dc,_,v,lv,le) =>
	    (cdcon dc; newv lv; cexp uvs le; if used lv then use v else ())

	  | F.RECORD (_,vs,lv,le) =>
	    (newv lv; cexp uvs le; if used lv then app use vs else ())

	  | F.SELECT (v,_,lv,le) =>
	    (newv lv; cexp uvs le; if used lv then use v else ())

	  | F.RAISE (v,_) => use v
	  | F.HANDLE (le,v) => (use v; cexp uvs le)
	  
	  | F.BRANCH (po,vs,le1,le2) =>
	    (app use vs; cpo po; cexp uvs le1; cexp uvs le2)
	  
	  | F.PRIMOP (po,vs,lv,le) =>
	    (newv lv; cexp uvs le;
	     if impurePO po orelse used lv then (cpo po; app use vs) else ())
	  
	  | le => buglexp("unexpected lexp", le)
in
    cexp
end

(* The code is almost the same for uncounting, except that calling
 * undertaker should not be done for non-free variables.  For that we
 * artificially increase the usage count of each variable when it's defined
 * (accomplished via the "def" calls)
 * so that its counter never reaches 0 while processing its scope.
 * Once its scope has been processed, we can completely get rid of
 * the variable and corresponding info (after verifying that the count
 * is indeed exactly 1 (accomplished by the "kill" calls) *)
fun unuselexp undertaker = let
    (* val use = if inc then use else unuse *)
    fun uncall lv = unuse undertaker true lv
    val unuse = fn F.VAR lv => unuse undertaker false lv | _ => ()
    val def = use false
    fun id x = x

    fun impurePO po = true		(* if a PrimOP is pure or not *)

    fun cpo (NONE:F.dict option,po,lty,tycs) = ()
      | cpo (SOME{default,table},po,lty,tycs) =
	(unuse(F.VAR default); app (unuse o F.VAR o #2) table)
    fun cdcon (s,Access.EXN(Access.LVAR lv),lty) = unuse(F.VAR lv)
      | cdcon _ = ()

    fun cfun (f,args,body) = (* census of a fundec *)
	(app def args;
	 inside f (fn()=> cexp body);
	 app kill args)

    and cexp lexp =
	case lexp
	 of F.RET vs => app unuse vs

	  | F.LET (lvs,le1,le2) =>
	    (app def lvs; cexp le2; cexp le1; app kill lvs)

	  | F.FIX (fs,le) =>
	    let val usedfs = (List.filter (used o #2) fs)
	    in app (def o #2) fs;
		cexp le;
		app (fn (_,lv,args,le) => cfun(lv, map #1 args, le)) usedfs;
		app (kill o #2) fs
	    end
	       
	  | F.APP (F.VAR f,vs) =>
	    (uncall f; app unuse vs)

	  | F.TFN ((tf,args,body),le) =>
	    (if used tf then inside tf (fn()=> cexp body) else ();
	     def tf; cexp le; kill tf)

	  | F.TAPP (F.VAR tf,tycs) => uncall tf

	  | F.SWITCH (v,cs,arms,default) =>
	    (unuse v; Option.map cexp default;
	     (* here we don't absolutely have to keep track of vars bound within
	      * each arm since these vars can't be eliminated anyway *)
	     app (fn (F.DATAcon(dc,_,lv),le) =>
		  (cdcon dc; def lv; cexp le; kill lv)
		   | (_,le) => cexp le)
		 arms)
		
	  | F.CON (dc,_,v,lv,le) =>
	    (cdcon dc; if used lv then unuse v else ();
	     def lv; cexp le; kill lv)

	  | F.RECORD (_,vs,lv,le) =>
	    (if used lv then app unuse vs else ();
	     def lv; cexp le; kill lv)

	  | F.SELECT (v,_,lv,le) =>
	    (if used lv then unuse v else ();
	     def lv; cexp le; kill lv)

	  | F.RAISE (v,_) => unuse v
	  | F.HANDLE (le,v) => (unuse v; cexp le)
	  
	  | F.BRANCH (po,vs,le1,le2) =>
	    (app unuse vs; cpo po; cexp le1; cexp le2)
	  
	  | F.PRIMOP (po,vs,lv,le) =>
	    (if impurePO po orelse used lv then (cpo po; app unuse vs) else ();
	     def lv; cexp le; kill lv)

	  | le => buglexp("unexpected lexp", le)
in
    (cexp, cfun)
end

val uselexp = census new use NONE

fun collect (fdec as (_,f,_,_)) =
    (M.clear m;				(* start from a fresh state *)
     uselexp (F.FIX([fdec], F.RET[F.VAR f])))

end
end
