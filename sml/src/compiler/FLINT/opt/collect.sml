(* copyright 1998 YALE FLINT PROJECT *)
(* monnier@cs.yale.edu *)

signature COLLECT =
sig
    type info
    
    (* Collect information about variables and function uses.
     * The info is accumulated in the map `m' *)
    val collect : FLINT.fundec -> FLINT.fundec

    val get : FLINT.lvar -> info

    (* query functions *)
    val escaping  : info -> bool	(* non-call uses *)
    val called    : info -> bool	(* known call uses *)
    val usenb     : info -> int	(* nb of non-recursive uses *)
    val actuals   : info -> (FLINT.value option list) (* constant args *)

    (* inc the "true=call,false=use" count *)
    val use    : FLINT.value list option -> info -> unit
    (* dec the "true=call,false=use" count and call the function (and return true) if zero *)
    val unuse  : (FLINT.lvar -> unit) -> bool -> FLINT.lvar -> bool
    (* transfer the counts of var1 to var2 *)
    val transfer : FLINT.lvar * FLINT.lvar -> unit
    (* add the counts of var1 to var2 *)
    (*  val addto  : info * info -> unit *)
    (* delete the last reference to a variable *)
    val kill   : FLINT.lvar -> unit
    (* create a new var entry (SOME arg list if fun) initialized to zero *)
    val new    : FLINT.lvar list option -> FLINT.lvar -> info

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
    (* function to copy (and collect info) a lexp *)
    val copylexp : FLINT.lvar IntmapF.intmap  -> FLINT.lexp -> FLINT.lexp

    (* mostly useful for PPFlint *)
    val LVarString : FLINT.lvar -> string
end

(* Internal vs External references:
 * I started with a version that kept track separately of internal and external
 * uses.  This has the advantage that if the extuses count goes to zero, we can
 * consider the function as dead.  Without this, recursive functions can never
 * be recognized as dead during fcontract (they are still eliminated at the
 * beginning, tho).  This looks nice at first, but poses problems:
 * - when you do simple inlining (just moving the body of the procedure), you
 *   may inadvertently turn ext-uses into int-uses.  This only happens when
 *   inlining mutually recursive function, but this can be commen (thing of
 *   when fcontract undoes a useless uncurrying or a recursive function).  This
 *   can be readily overcome by not using the `move body' optimization in
 *   dangerous cases and do the full copy+kill instead.
 * - you have to keep track of what is inside what.  The way I did it was to
 *   have an 'inside' ref cell in each fun.  That was a bad idea.  The problem
 *   stems from the fact that when you detect that a function becomes dead,
 *   you have to somehow reset those `inside' ref cells to reflect the location
 *   of the function before you can uncount its references.  In most cases, this
 *   is unnecessary, but it is necessary when undertaking a function mutually
 *   recursive with a function in which you currently are when you detect the
 *   function's death.
 * rather than fix this last point, I decided to get rid of the distinction.
 * This makes the code simpler and less bug-prone at the cost of slightly
 * increasing the number of fcontract passes required.
 *)

structure Collect :> COLLECT =
struct
local
    structure F  = FLINT
    structure M  = Intmap
    structure FU = FlintUtil
    structure LV = LambdaVar
    structure PP = PPFlint
in

val say = Control.Print.say
fun bug msg = ErrorMsg.impossible ("Collect: "^msg)
fun buglexp (msg,le) = (say "\n"; PP.printLexp le; say " "; bug msg)
fun bugval (msg,v) = (say "\n"; PP.printSval v; say " "; bug msg)
fun ASSERT (true,_) = ()
  | ASSERT (FALSE,msg) = bug ("assertion "^msg^" failed")

exception UnexpectedTransfer

datatype info
  (* for functions we keep track of calls and escaping uses *)
  = Fun of {calls: int ref, uses: int ref,
	    args: (FLINT.lvar * (FLINT.value option)) option list ref}
  | Var of int ref	(* for other vars, a simple use count is kept *)
  | Transfer of FLINT.lvar	(* for vars which have been transfered *)
    
exception NotFound
	      
val m : info M.intmap = M.new(128, NotFound)

(* map related helper functions *)
fun get lv = (M.map m lv)
		  handle x as NotFound =>
		  (say ("Collect: ERROR: get unknown var "^
			(LV.lvarName lv)^
			". Pretending dead...\n");
		   (*  raise x; *)
		   Var (ref 0))

fun LVarString lv =
    (LV.lvarName lv)^
    ((case get lv of
	 Var uses => "{"^(Int.toString (!uses))^"}"
       | Fun {calls,uses,...} =>
	 concat ["{", Int.toString (!uses), ",", Int.toString (!calls), "}"]
       | Transfer lv => "{->"^(LVarString lv)^"}")
	 handle NotFound => "{?}")

fun get' lv = case get lv of Transfer nlv => get' nlv | i => i

fun new args lv =
    let val i =
	    case args
	     of SOME args => Fun{calls=ref 0, uses=ref 0,
				 args=ref (map (fn a => SOME(a, NONE)) args)}
	      | NONE => Var(ref 0)
    in M.add m (lv, i); i
    end

(* adds the counts of lv1 to those of lv2 *)
fun addto (info1,info2) =
    case info1
     of Var uses1 =>
	(case info2
	  of Var uses2 => uses2 := !uses2 + !uses1
	   | Fun {uses=uses2,...} => uses2 := !uses2 + !uses1
	   | Transfer _ => raise UnexpectedTransfer)
      | Fun {uses=uses1,calls=calls1,...} =>
	(case info2
	  of Fun {uses=uses2,calls=calls2,...} =>
	     (uses2 := !uses2 + !uses1; calls2 := !calls2 + !calls1)
	   | Var uses2 => uses2 := !uses2 + !uses1
	   | Transfer _ => raise UnexpectedTransfer)
      | Transfer _ => raise UnexpectedTransfer

fun transfer (lv1,lv2) =
    (addto(get lv1, get lv2);
     (* note the transfer *)
     M.add m (lv1, Transfer lv2)) handle x => raise x
	       
fun inc ri = (ri := !ri + 1)
fun dec ri = (ri := !ri - 1)

(* - first list is list of formal args
 * - second is list of `up to know known arg'
 * - third is args of the current call. *)
fun mergearg (NONE,a) = NONE
  | mergearg (SOME(fv,NONE),a) =
    if a = F.VAR fv then SOME(fv,NONE) else SOME(fv,SOME a)
  | mergearg (SOME(fv,SOME b),a) =
    if a = b orelse a = F.VAR fv then SOME(fv,SOME b) else NONE

fun actuals (Var _) = bug "can't query actuals of a var" (* (LVarString lv) *)
  | actuals (Transfer lv) = raise UnexpectedTransfer
  | actuals (Fun{args,...}) = map (fn SOME(_,v) => v | _ => NONE) (!args)

fun use call (Var uses) = inc uses
  | use call (Transfer lv) = raise UnexpectedTransfer
  | use call (Fun {uses,calls,args,...}) =
    case call of
	NONE => (inc uses; args := map (fn _ => NONE) (!args))
      | SOME vals => 
	(inc calls; inc uses; args := ListPair.map mergearg (!args, vals))

fun unuse undertaker call lv =
    let fun check uses =
	    if !uses < 0 then
		bugval("decrementing too much", F.VAR lv)
	    else if !uses = 0 then
		(undertaker lv; true)
	    else false
    in case get' lv
	of Var uses => (dec uses; check uses)
	 | Fun {uses,calls,...} =>
	   (dec uses; if (call andalso !calls > 0) then dec calls else ASSERT(!uses >= !calls, "unknown sanity"); check uses)
	 | Transfer lv => bug "transfer"
    end handle x => raise x

fun usenb (Fun{uses=uses,...} | Var uses) = !uses
  | usenb (Transfer _) = (raise UnexpectedTransfer; 0)
fun used i      = usenb i > 0

fun escaping (Fun{uses,calls,...}) = !uses > !calls
  | escaping (Var us) = !us > 0 (* arbitrary, but hopefully "safe" *)
  | escaping (Transfer lv) = raise UnexpectedTransfer

fun called (Fun{calls,...}) = !calls > 0
  | called (Var us) = false (* arbitrary, but consistent with escaping *)
  | called (Transfer lv) = raise UnexpectedTransfer

(* Ideally, we should check that usenb = 1, but we may have been a bit
 * conservative when keeping the counts uptodate *)
fun kill lv = (ASSERT(usenb(get' lv) >= 1, "usenb "^(LVarString lv)^" >= 1 ");
	       M.rmv m lv)

(* ********************************************************************** *)
(* ********************************************************************** *)

datatype usage
  = All
  | None
  | Some of bool list

fun usage bs =
    let fun ua [] = All
	  | ua (false::_) = Some bs
	  | ua (true::bs) = ua bs
	fun un [] = None
	  | un (true::_) = Some bs
	  | un (false::bs) = un bs
    in case bs
	of true::bs => ua bs
	 | false::bs => un bs
	 | [] => None
    end

fun impurePO po = true		(* if a PrimOP is pure or not *)

val census = let
    (* val use = if inc then use else unuse *)
    fun call args lv = use args (get' lv)
    val use = fn F.VAR lv => use NONE (get' lv) | _ => ()
    fun newv lv = new NONE lv
    fun newf args lv = new args lv
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
	(case lexp
	 of F.RET vs => app use vs
(* 	    (case uvs *)
(* 	      of SOME uvs => (* only count vals that are actually used *) *)
(* 		 app (fn(v,uv)=>if uv then use v else ()) (ListPair.zip(vs,uvs)) *)
(* 	       | NONE => app use vs) *)

	  | F.LET (lvs,le1,le2) =>
	    let val lvsi = map newv lvs
	    in cexp uvs le2; cexp (usage(map used lvsi)) le1
	    end

	  | F.FIX (fs,le) =>
	    let val fs = map (fn (_,f,args,body) =>
			      (newf (SOME(map #1 args)) f,args,body))
			     fs
		fun cfun (_,args,body) = (* census of a fundec *)
		    (app (fn (v,t) => ignore(newv v)) args; cexp All body)
		fun cfix fs =	(* census of a list of fundecs *)
		    let val (ufs,nfs) = List.partition (used o #1) fs
		    in if List.null ufs then ()
		       else (app cfun ufs; cfix nfs)
		    end
	    in cexp uvs le; cfix fs
	    end
	       
	  | F.APP (F.VAR f,vs) =>
	    (call (SOME vs) f; app use vs)

	  | F.TFN ((tf,args,body),le) =>
	    let val tfi = newf NONE tf
	    in cexp uvs le;
		if used tfi then cexp All body else ()
	    end

	  | F.TAPP (F.VAR tf,tycs) => call NONE tf

	  | F.SWITCH (v,cs,arms,def) =>
	    (use v; Option.map (cexp uvs) def;
	     app (fn (F.DATAcon(dc,_,lv),le) => (cdcon dc; newv lv; cexp uvs le)
		   | (_,le) => cexp uvs le)
		 arms)
		
	  | F.CON (dc,_,v,lv,le) =>
	    let val lvi = newv lv
	    in cdcon dc; cexp uvs le; if used lvi then use v else ()
	    end

	  | F.RECORD (_,vs,lv,le) =>
	    let val lvi = newv lv
	    in cexp uvs le; if used lvi then app use vs else ()
	    end

	  | F.SELECT (v,_,lv,le) =>
	    let val lvi = newv lv
	    in cexp uvs le; if used lvi then use v else ()
	    end

	  | F.RAISE (v,_) => use v
	  | F.HANDLE (le,v) => (use v; cexp uvs le)
	  
	  | F.BRANCH (po,vs,le1,le2) =>
	    (app use vs; cpo po; cexp uvs le1; cexp uvs le2)
	  
	  | F.PRIMOP (po,vs,lv,le) =>
	    let val lvi = newv lv
	    in cexp uvs le;
		if impurePO po orelse used lvi then (cpo po; app use vs) else ()
	    end
	  
	  | le => buglexp("unexpected lexp", le)) handle x => raise x
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
    fun uncall lv = ignore(unuse undertaker true lv)
    val unuse = fn F.VAR lv => ignore(unuse undertaker false lv) | _ => ()
    fun def lv = (use NONE (get lv)) handle x => raise x
    fun id x = x

    fun cpo (NONE:F.dict option,po,lty,tycs) = ()
      | cpo (SOME{default,table},po,lty,tycs) =
	(unuse(F.VAR default); app (unuse o F.VAR o #2) table)
    fun cdcon (s,Access.EXN(Access.LVAR lv),lty) = unuse(F.VAR lv)
      | cdcon _ = ()

    fun cfun (f,args,body) = (* census of a fundec *)
	(app def args; cexp body; app kill args) handle x => raise x

    and cexp lexp =
	(case lexp
	 of F.RET vs => app unuse vs

	  | F.LET (lvs,le1,le2) =>
	    (app def lvs; cexp le2; cexp le1; app kill lvs)

	  | F.FIX (fs,le) =>
	    let val usedfs = (List.filter ((used o get o #2) handle x => raise x) fs)
	    in app (def o #2) fs;
		cexp le;
		app (fn (_,lv,args,le) => cfun(lv, map #1 args, le)) usedfs;
		app (kill o #2) fs
	    end
	       
	  | F.APP (F.VAR f,vs) =>
	    (uncall f; app unuse vs)

	  | F.TFN ((tf,args,body),le) =>
	    (if used(get tf) then cexp body else ();
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
	    (cdcon dc; if used(get lv) then unuse v else ();
	     def lv; cexp le; kill lv)

	  | F.RECORD (_,vs,lv,le) =>
	    (if used(get lv) then app unuse vs else ();
	     def lv; cexp le; kill lv)

	  | F.SELECT (v,_,lv,le) =>
	    (if used(get lv) then unuse v else ();
	     def lv; cexp le; kill lv)

	  | F.RAISE (v,_) => unuse v
	  | F.HANDLE (le,v) => (unuse v; cexp le)
	  
	  | F.BRANCH (po,vs,le1,le2) =>
	    (app unuse vs; cpo po; cexp le1; cexp le2)
	  
	  | F.PRIMOP (po,vs,lv,le) =>
	    (if impurePO po orelse used(get lv) then
		 (cpo po; app unuse vs)
	     else ();
	     def lv; cexp le; kill lv)

	  | le => buglexp("unexpected lexp", le)) handle x => raise x
in
    (cexp, cfun)
end

val uselexp = census All
fun copylexp alpha le =
    let val nle = FU.copy alpha le
    in uselexp nle; nle
    end

fun collect (fdec as (_,f,_,_)) =
    ((*  say "Entering Collect...\n"; *)
     M.clear m;				(* start from a fresh state *)
     PP.LVarString := LVarString;
     uselexp (F.FIX([fdec], F.RET[F.VAR f]));
     (*  say "...Collect Done.\n"; *)
     fdec)

end
end
