(* copyright 1998 YALE FLINT PROJECT *)
(* monnier@cs.yale.edu *)

signature COLLECT =
sig
    type info
    
    (* Collect information about variables and function uses.
     * The info is accumulated in the map `m' *)
    val collect : FLINT.fundec -> FLINT.fundec

(*      val get : FLINT.lvar -> info *)

    (* query functions *)
    val escaping  : FLINT.lvar -> bool	(* non-call uses *)
    val called    : FLINT.lvar -> bool	(* known call uses *)
    val usenb     : FLINT.lvar -> int	(* nb of non-recursive uses *)
    val actuals   : FLINT.lvar -> (FLINT.value option list) (* constant args *)

    (* inc the "true=call,false=use" count *)
    val use    : FLINT.value list option -> FLINT.lvar -> unit
    (* dec the "true=call,false=use" count and call the function if zero *)
    val unuse  : (FLINT.lvar -> unit) -> bool -> FLINT.lvar -> unit
    (* transfer the counts of var1 to var2 *)
    val transfer : FLINT.lvar * FLINT.lvar -> unit
    (* add the counts of var1 to var2 *)
    val addto  : FLINT.lvar * FLINT.lvar -> unit
    (* delete the last reference to a variable *)
    val kill   : FLINT.lvar -> unit
    (* create a new var entry (SOME arg list if fun) initialized to zero *)
    val new    : FLINT.lvar list option -> FLINT.lvar -> unit

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
 (*     val uselexp : FLINT.lexp -> unit *)
    (* function to collect info about a newly created lexp *)
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
    structure FM = IntmapF
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
  (* for functions we keep track of calls and escaping uses *)
  = Fun of {calls: int ref, uses: int ref, int: int ref,
	    args: (FLINT.lvar * (FLINT.value option)) option list ref}
  | Var of int ref	(* for other vars, a simple use count is kept *)
  | Transfer of FLINT.lvar	(* for vars which have been transfered *)
    
exception NotFound
	      
val m : info M.intmap = M.new(1024, NotFound)

(* map related helper functions *)
fun get lv = (M.map m lv)
		 (* handle x as NotFound =>
		 (say "\nCollect:get unknown var ";
		  PP.printSval (F.VAR lv);
		  say ". Assuming dead...";
		  raise x;
		  Var (ref 0)) *)

fun new (SOME args) lv =
    M.add m (lv, Fun{calls=ref 0, uses=ref 0, int=ref 0,
		     args=ref (map (fn a => SOME(a, NONE)) args)})
  | new NONE lv = M.add m (lv, Var(ref 0))

fun LVarString lv =
    (LV.lvarName lv)^
    ((case get lv of
	 Var uses => "{"^(Int.toString (!uses))^"}"
       | Fun {calls,uses,...} =>
	 concat ["{", Int.toString (!calls), ",", Int.toString (!uses), "}"]
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
	      | Fun {uses=uses2,...} => uses2 := !uses2 + !uses1
	      | Transfer _ => bugval("transfering to a Transfer", F.VAR lv2))
	 | Fun {uses=uses1,calls=calls1,...} =>
	   (case info2
	     of Fun {uses=uses2,calls=calls2,...} =>
		(uses2 := !uses2 + !uses1; calls2 := !calls2 + !calls1)
	      | Var uses2 => uses2 := !uses2 + !uses1
	      | Transfer _ => bugval("transfering to a Transfer", F.VAR lv2))
	 | Transfer _ => bugval("transfering from a Transfer", F.VAR lv1)
    end
fun transfer (lv1,lv2) =
    (addto(lv1, lv2); M.add m (lv1, Transfer lv2))	(* note the transfer *)
	       
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

fun actuals lv =
    case get lv
     of Var _ => bug ("can't query actuals of var "^(LVarString lv))
      | Transfer lv => actuals lv
      | Fun{args,...} => map (fn SOME(_,v) => v | _ => NONE) (!args)

fun use call lv =
    case get lv
     of Var uses => inc uses
      | Transfer lv => use call lv
      | Fun {uses,calls,args,...} =>
	case call of
	    NONE => (inc uses; args := map (fn _ => NONE) (!args))
	  | SOME vals => 
	    (inc calls; inc uses; args := ListPair.map mergearg (!args, vals))

fun unuse undertaker call lv =
    let fun check uses =
	    if !uses < 0 then
		bugval("decrementing too much", F.VAR lv)
	    else if !uses = 0 then
		(*  if lv = 1294 then bug "here it is !!" else *) undertaker lv
	    else ()
    in case get lv
	of Var uses => (dec uses; check uses)
	 | Fun {uses,calls,...} =>
	   (dec uses; if call then dec calls else ASSERT(!uses >= !calls, "unknown sanity"); check uses)
	 | Transfer lv => unuse undertaker call lv
    end

fun usenb lv     = case get lv of (Fun{uses=uses,...} | Var uses) => !uses
				| Transfer _ => 0
fun used lv      = usenb lv > 0

fun escaping lv =
    case get lv
     of Fun{uses,calls,...} => !uses > !calls
      | Var us => !us > 0 (* arbitrary, but I opted for the "safe" choice *)
      | Transfer lv => (say "\nCollect escaping on transfer"; escaping lv)

fun called lv =
    case get lv
     of Fun{calls,...} => !calls > 0
      | Var us => false (* arbitrary, but consistent with escaping *)
      | Transfer lv => (say "\nCollect escaping on transfer"; called lv)

(* Ideally, we should check that usenb = 1, but we may have been a bit
 * conservative when keeping the counts uptodate *)
fun kill lv = (ASSERT(usenb lv >= 1, concat ["usenb lv >= 1 ", !PP.LVarString lv]); M.rmv m lv)

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

val cplv = LambdaVar.dupLvar

fun impurePO po = true		(* if a PrimOP is pure or not *)

fun census newv substvar alpha uvs le = let
    val cexp = census newv substvar
    val usevar = substvar NONE alpha
    fun callvar args lv = substvar (SOME args) alpha lv
    fun use (F.VAR lv) = F.VAR(usevar lv) | use v = v
    fun call args (F.VAR lv) = F.VAR(callvar args lv) | call _ v = v
    fun newvs (lvs,alpha) =
	foldr (fn (lv,(lvs,alpha)) =>
	       let val (nlv,nalpha) = newv NONE (lv,alpha)
	       in (nlv::lvs, nalpha) end)
	      ([],alpha) lvs
    fun newfs (fdecs,alpha) =
	foldr (fn ((_,lv,args,_):F.fundec,(lvs,alpha)) =>
	       let val (nlv,nalpha) = newv (SOME(map #1 args)) (lv,alpha)
	       in (nlv::lvs, nalpha) end)
	      ([],alpha) fdecs
    fun cdcon (s,Access.EXN(Access.LVAR lv),lty) =
	(s, Access.EXN(Access.LVAR(usevar lv)), lty)
      | cdcon dc = dc
    fun cpo (SOME{default,table},po,lty,tycs) =
	(SOME{default=usevar default,
	      table=map (fn (tycs,lv) => (tycs, usevar lv)) table},
	 po,lty,tycs)
      | cpo po = po
in case le
    of F.RET vs => F.RET(map use vs)

     | F.LET (lvs,le,body) =>
       let val (nlvs,nalpha) = newvs (lvs,alpha)
	   val nbody = cexp nalpha uvs body
	   val nuvs = usage(map used nlvs)
	   val nle = cexp alpha nuvs le
       in F.LET(nlvs, nle, nbody)
       end

     | F.FIX (fdecs,le) =>
       let val (nfs, nalpha) = newfs(fdecs, alpha)

	   (* census of a function *)
	   fun cfun ((fk,f,args,body):F.fundec,nf) =
	       let val (nargs,ialpha) = newvs(map #1 args, nalpha)
		   val nbody = cexp ialpha All body
	       in (fk, nf, ListPair.zip(nargs, (map #2 args)), nbody)
	       end

	   (* some sort of tracing GC on functions *)
	   fun cfix fs = let
	       val (ufs,nfs) = List.partition (used o #2) fs
	   in if List.null ufs then []
	      else (map cfun ufs) @ (cfix nfs)
	   end

	   val nle = cexp nalpha uvs le
	   val nfdecs = cfix(ListPair.zip(fdecs, nfs))
       in
	   if List.null nfdecs then nle else F.FIX(nfdecs, nle)
       end

     | F.APP (f,args) => F.APP(call args f, map use args)

     | F.TFN ((lv,args,body),le) =>
       (* don't forget to rename the tvar also *)
       let val (nlv,nalpha) = newv (SOME[]) (lv,alpha)
	   val nle = cexp nalpha uvs le
       in
	   if used nlv then
	       let val (nargs,ialpha) = newvs(map #1 args, alpha)
		   val nbody = cexp ialpha All body
	       in F.TFN((nlv, ListPair.zip(nargs, map #2 args), nbody), nle)
	       end
	   else
	       nle
       end

     | F.TAPP (f,tycs) => F.TAPP(call [] f, tycs)

     | F.SWITCH (v,ac,arms,def) =>
       let fun carm (F.DATAcon(dc,tycs,lv),le) =
	       let val (nlv,nalpha) = newv NONE (lv, alpha)
	       in (F.DATAcon(cdcon dc, tycs, nlv), cexp nalpha uvs le)
	       end
	     | carm (con,le) = (con, cexp alpha uvs le)
       in F.SWITCH(use v, ac, map carm arms, Option.map (cexp alpha uvs) def)
       end

     | F.CON (dc,tycs,v,lv,le) =>
       let val (nlv,nalpha) = newv NONE (lv, alpha)
	   val nle = cexp nalpha uvs le
       in if used nlv
	  then F.CON(cdcon dc, tycs, use v, nlv, nle)
	  else nle
       end

     | F.RECORD (rk,vs,lv,le) => 
       let val (nlv,nalpha) = newv NONE (lv, alpha)
	   val nle = cexp nalpha uvs le
       in if used nlv
	  then F.RECORD(rk, map use vs, nlv, nle)
	  else nle
       end

     | F.SELECT (v,i,lv,le) => 
       let val (nlv,nalpha) = newv NONE (lv, alpha)
	   val nle = cexp nalpha uvs le
       in if used nlv
	  then F.SELECT(use v, i, nlv, nle)
	  else nle
       end

     | F.RAISE (v,ltys) => F.RAISE(use v, ltys)

     | F.HANDLE (le,v) => F.HANDLE(cexp alpha uvs le, use v)

     | F.BRANCH (po,vs,le1,le2) =>
       F.BRANCH(cpo po, map use vs, cexp alpha uvs le1, cexp alpha uvs le2)

     | F.PRIMOP (po,vs,lv,le) =>
       let val (nlv,nalpha) = newv NONE (lv, alpha)
	   val nle = cexp nalpha uvs le
       in if impurePO po orelse used nlv
	  then F.PRIMOP(cpo po, map use vs, nlv, nle)
	  else nle
       end
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
    val def = use NONE
    fun id x = x

    fun cpo (NONE:F.dict option,po,lty,tycs) = ()
      | cpo (SOME{default,table},po,lty,tycs) =
	(unuse(F.VAR default); app (unuse o F.VAR o #2) table)
    fun cdcon (s,Access.EXN(Access.LVAR lv),lty) = unuse(F.VAR lv)
      | cdcon _ = ()

    fun cfun (f,args,body) = (* census of a fundec *)
	(app def args; cexp body; app kill args)

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
	    (if used tf then cexp body else ();
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

fun uselexp le =
    let fun new' call (lv,alpha) = (new call lv; (lv,alpha))
	fun use' call alpha lv = (use call lv; lv)
    in census new' use' () All le
    end

(*  fun uselexp le = (uselexp' le; ()) *)

fun copylexp alpha le =
    let fun new' call (lv,alpha) =
	    let val nlv = cplv lv
	    in new call nlv; (nlv, FM.add(alpha, lv, nlv))
	    end
	fun use' call alpha lv =
	    let val nlv = (FM.lookup alpha lv) handle FM.IntmapF => lv
	    in use call nlv; nlv
	    end
    in census new' use' alpha All le
    end

fun collect (fdec as (_,f,_,_)) =
    let val _ = M.clear m		(* start from a fresh state *)
	val nle = uselexp (F.FIX([fdec], F.RET[F.VAR f]))
    in case nle of
	F.FIX([nfdec], F.RET[F.VAR g]) => (ASSERT(f = g, "f = g"); nfdec)
      | _ => bug "not an fdec anymore"
    end

end
end
