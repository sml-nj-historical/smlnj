(* copyright 1998 YALE FLINT PROJECT *)
(* monnier@cs.yale.edu *)

signature FCONTRACT =
sig
    
    (* needs Collect to be setup properly *)
    val contract : FLINT.fundec -> FLINT.fundec
	
end

(* All kinds of beta-reductions.  In order to do as much work per pass as
 * possible, the usage counts of each variable (maintained by the Collect
 * module) is kept as much uptodate as possible.  For instance as soon as a
 * variable becomes dead, all the variables that were referenced have their
 * usage counts decremented correspondingly.  This means that we have to
 * be careful to make sure that a dead variable will indeed not appear
 * in the output lexp since it might else reference other dead variables *)

(* things that fcontract does:
 * - several things not mentioned
 * - elimination of Con(Decon x)
 * - update counts when selecting a SWITCH alternative
 * - contracting RECORD(R.1,R.2) => R  (only if the type is easily available)
 * - dropping of dead arguments
 * - elimination of constant arguments
 *)

(* things that lcontract.sml does that fcontract doesn't do (yet):
 * - inline across DeBruijn depths (will be solved by named-tvar)
 * - elimination of let [dead-vs] = pure in body
 *)

(* things that cpsopt/eta.sml did that fcontract doesn't do:
 * - let f vs = select(v,i,g,g vs)
 *)

(* things that cpsopt/contract.sml did that fcontract doesn't do:
 * - IF-idiom (I still don't know what it is)
 * - unifying branches
 * - Handler operations
 * - primops expressions
 * - branch expressions
 *)

(* things that could also be added:
 * - elimination of dead vars in let
 *)

(* things that would require some type info:
 * - dropping foo in LET vs = RAISE v IN foo
 *)

(* eta-reduction is tricky:
 * - recognition of eta-redexes and introduction of the corresponding
 *   substitution in the table has to be done at the very beginning of
 *   the processing of the FIX
 * - eta-reduction can turn a known function into an escaping function
 * - fun f (g,v2,v3) = g(g,v2,v3) looks tremendously like an eta-redex
 *)

(* order of contraction is important:
 * - the body of a FIX is contracted before the functions because the
 *   functions might end up being inlined in the body in which case they
 *   could be contracted twice.
 *)

(* When creating substitution f->g (as happens with eta redexes or with
 * code like `LET [f] = RET[g]'), we need to make sure that the usage cout
 * of f gets properly transfered to g.  One way to do that is to make the
 * transfer incremental:  each time we apply the substitution, we decrement
 * f's count and increment g's count.  But this can be tricky since the
 * elimination of the eta-redex (or the trivial binding) eliminates one of the
 * references to g and if this is the only one, we might trigger the killing
 * of g even though its count would be later incremented.  Similarly, inlining
 * of g would be dangerous as long as some references to f exist.
 * So instead we do the transfer once and for all when we see the eta-redex,
 * which frees us from those two problems but forces us to make sure that
 * every existing reference to f will be substituted with g.
 * Also, the transfer of counts from f to g is not quite straightforward
 * since some of the references to f might be from inside g and without doing
 * the transfer incrementally, we can't easily know which of the usage counts
 * of f should be transfered to the internal counts of g and which to the
 * external counts.
 *)

(* Preventing infinite inlining:
 * - inlining a function in its own body amounts to unrolling which has
 *   to be controlled (you only want to unroll some number of times).
 *   It's currently simply not allowed.
 * - inlining a recursive function outside of tis body amounts to `peeling'
 *   one iteration. Here also, since the inlined body will have yet another
 *   call, the inlining risks non-termination.  It's hence also
 *   not allowed.
 * - inlining a mutually recursive function is just a more general form
 *   of the problem above although it can be safe and desirable in some cases.
 *   To be safe, you simply need that one of the functions forming the
 *   mutual-recursion loop cannot be inlined (to break the loop).  This cannot
 *   be trivially checked.  So we (foolishly?) trust the `inline' bit in
 *   those cases.  This is mostly used to inline wrappers inside the
 *   function they wrap.
 * - even if one only allows inlining of funtions showing no sign of
 *   recursion, we can be bitten by a program creating its own Y combinator:
 *       datatype dt = F of dt -> int -> int
 *       let fun f (F g) x = g (F g) x in f (F f) end
 *   To solve this problem, `cexp' has an `ifs' parameter containing the set
 *   of funtions that we are inlining in order to detect (and break) cycles.
 * - funnily enough, if we allow inlining recursive functions the cycle
 *   detection will ensure that the unrolling (or peeling) will only be done
 *   once.  In the future, maybe.
 *)

(* Dropping useless arguments.
 * Arguments whose value is constant (i.e. the function is known and each
 * call site provides the same value for that argument (or the argument
 * itself in the case of recursive calls) can be safely removed and replaced
 * inside the body by a simple let binding.  The only problem is that the
 * constant argument might be out of scope at the function definition site.
 * It is obviously always possible to move the function to bring the argument
 * in scope, but since we don't do any code motion here, we're stuck.
 * If it wasn't for this little problem, we could do the cst-arg removal in
 * collect (we don't gain anything from doing it here).
 * The removal of dead arguments (args not used in the body) on the other
 * hand can quite well be done in collect, the only problem being that it
 * is convenient to do it after the cst-arg removal so that we can rely
 * on deadarg to do the actual removal of the cst-arg.
 *)

(* Simple inlining (inlining called-once functions, which doesn't require
 * alpha-renaming) seems inoffensive enough but is not always desirable.
 * The typical example is wrapper functions introduced by eta-expand: they
 * usually (until inlined) contain the only call to the main function,
 * but inlining the main function in the wrapper defeats the purpose of the
 * wrapper.
 * cpsopt dealt with this problem by adding a `NO_INLINE_INTO' hint to the
 * wrapper function.  In this file, the idea is the following:
 * If you have a function declaration like `let f x = body in exp', first
 * contract `exp' and only contract `body' afterwards.  This ensures that
 * the eta-wrapper gets a chance to be inlined before it is (potentially)
 * eta-reduced away.  Interesting details:
 * - all functions (even the ones that would have a `NO_INLINE_INTO') are
 *   contracted, because the "aggressive usage count maintenance" makes any
 *   alternative painful (the collect phase has already assumed that dead code
 *   will be eliminated, which means that fcontract should at the very least
 *   do the dead-code elimination, so you can only avoid fcontracting a
 *   a function if you can be sure that the body doesn't contain any dead-code,
 *   which is generally  not known).
 * - once a function is fcontracted it is marked as non-inlinable since
 *   fcontraction might have changed its shape considerably (via inlining).
 *   This means that in the case of
 *       let fwrap x = body1 and f y = body2 in exp
 *   if fwrap is fcontracted before f, then fwrap cannot be inlined in f.
 *   To minimize the impact of this problem, we make sure that we fcontract
 *   inlinable functions only after fcontracting other mutually recursive
 *   functions.
 * - at the very end of the optimization phase, cpsopt had a special pass
 *   that ignored the `NO_INLINE_INTO' hint (since at this stage, inlining
 *   into it doesn't have any undesirable side effects any more).  The present
 *   code doesn't need such a thing.  On another hand, the cpsopt approach
 *   had the advantage of keeping the `inline' bit from one contract phase to
 *   the next.  If this ends up being important, one could add a global
 *   "noinline" flag that could be set to true whenever fcontracting an
 *   inlinable function (this would ensure that fcontracting such an inlinable
 *   function can only reduce its size, which would allow keeping the `inline'
 *   bit set after fcontracting).
 *)

structure FContract :> FCONTRACT =
struct
local
    structure F  = FLINT
    structure M  = IntmapF
    structure S  = IntSetF
    structure C  = Collect
    structure O  = Option
    structure DI = DebIndex
    structure PP = PPFlint
    structure FU = FlintUtil
    structure LT = LtyExtern
    structure OU = OptUtils
    structure CTRL = Control.FLINT
in

val say = Control.Print.say
fun bug msg = ErrorMsg.impossible ("FContract: "^msg)
fun buglexp (msg,le) = (say "\n"; PP.printLexp le; bug msg)
fun bugval (msg,v) = (say "\n"; PP.printSval v; bug msg)

(* fun sayexn e = app say (map (fn s => s^" <- ") (SMLofNJ.exnHistory e)) *)

fun ASSERT (true,_) = ()
  | ASSERT (FALSE,msg) = bug ("assertion "^msg^" failed")

val cplv = LambdaVar.dupLvar

datatype sval
  = Val    of F.value			(* F.value should never be F.VAR lv *)
  | Fun    of F.lvar * F.lexp * (F.lvar * F.lty) list * F.fkind * DI.depth
  | TFun   of F.lvar * F.lexp * (F.tvar * F.tkind) list * DI.depth
  | Record of F.lvar * F.value list
  | Con    of F.lvar * F.value * F.dcon * F.tyc list
  | Decon  of F.lvar * F.value * F.dcon * F.tyc list
  | Select of F.lvar * F.value * int
  | Var    of F.lvar * F.lty option	(* cop out case *)

fun sval2lty (Var(_,x)) = x
  | sval2lty (Decon(_,_,(_,_,lty),tycs)) =
    SOME(hd(#2 (LT.ltd_arrow (hd(LT.lt_inst(lty, tycs))))))
  | sval2lty _ = NONE

fun tycs_eq ([],[]) = true
  | tycs_eq (tyc1::tycs1,tyc2::tycs2) =
    LT.tc_eqv(tyc1,tyc2) andalso tycs_eq(tycs1,tycs2)
  | tycs_eq _ = false

fun contract (fdec as (_,f,_,_)) = let

val inlineWitness = ref false

(* cfg: is used for deBruijn renumbering when inlining at different depths
 * ifs (inlined functions): records which functions we're currently inlining
 *     in order to detect loops
 * m: is a map lvars to their defining expressions (svals) *)
fun cexp (cfg as (d,od)) ifs m le cont = let

    val loop = cexp cfg ifs

    fun used lv = (C.usenb(C.get lv) > 0)
		      handle x =>
		      (say("while in FContract.used "^(C.LVarString lv)^"\n");
		       raise x)
    
    fun impurePO po = true		(* if a PrimOP is pure or not *)

    fun eqConV (F.INTcon i1,	F.INT i2)	= i1 = i2
      | eqConV (F.INT32con i1,	F.INT32 i2)	= i1 = i2
      | eqConV (F.WORDcon i1,	F.WORD i2)	= i1 = i2
      | eqConV (F.WORD32con i1,	F.WORD32 i2)	= i1 = i2
      | eqConV (F.REALcon r1,	F.REAL r2)	= r1 = r2
      | eqConV (F.STRINGcon s1,	F.STRING s2)	= s1 = s2
      | eqConV (con,v) = bugval("unexpected comparison with val", v)

    fun lookup m lv = (M.lookup m lv)
			  (* handle e as M.IntmapF =>
			  (say "\nlooking up unbound ";
			   say (!PP.LVarString lv);
			   raise e) *)

    fun sval2val sv =
	case sv
	 of (Fun{1=lv,...} | TFun{1=lv,...} | Record{1=lv,...} | Decon{1=lv,...}
	  | Con{1=lv,...} | Select{1=lv,...} | Var{1=lv,...}) => F.VAR lv
	  | Val v => v
			 
    fun val2sval m (F.VAR ov) = 
	((lookup m ov) handle x => ((*  PP.printSval(F.VAR ov); *) raise x))
      | val2sval m v = Val v

    fun bugsv (msg,sv) = bugval(msg, sval2val sv)

    fun subst m ov = sval2val (lookup m ov)
    val substval = sval2val o (val2sval m)
    fun substvar lv =
	case substval(F.VAR lv)
	 of F.VAR lv => lv
	  | v => bugval ("unexpected val", v)

    fun unuseval f (F.VAR lv) = ignore((C.unuse f false lv) handle x => raise x)
      | unuseval f _ = ()

    (* called when a variable becomes dead.
     * it simply adjusts the use-counts *)
    fun undertake m lv =
	let val undertake = undertake m
	in case lookup m lv
	    of Var {1=nlv,...}	 => ()
	     | Val v		 => ()
	     | Fun (lv,le,args,_,_) =>
	       (#2 (C.unuselexp undertake)) (lv, map #1 args, le)
	     | TFun{1=lv,2=le,...} => (#2 (C.unuselexp undertake)) (lv, [], le)
	     | (Select {2=v,...} | Con {2=v,...}) => unuseval undertake v
	     | Record {2=vs,...} => app (unuseval undertake) vs
	     (* decon's are implicit so we can't get rid of them *)
	     | Decon _ => ()
	end
		handle M.IntmapF =>
		(say("Unable to undertake "^(C.LVarString lv)^"\n"))
		     | x =>
		       (say("while undertaking "^(C.LVarString lv)^"\n"); raise x)

    fun addbind (m,lv,sv) = M.add(m, lv, sv)

    (* substitute a value sv for a variable lv and unuse value v. *)
    fun substitute (m, lv1, sv, v) =
	(case sval2val sv of F.VAR lv2 => C.transfer(lv1,lv2) | v2 => ();
	 unuseval (undertake m) v;
	 addbind(m, lv1, sv)) handle x =>
	     (say ("while substituting "^
		   (C.LVarString lv1)^
		   " -> ");
	      PP.printSval (sval2val sv);
	      raise x)

    (* common code for primops *)
    fun cpo (SOME{default,table},po,lty,tycs) =
	(SOME{default=substvar default,
	      table=map (fn (tycs,lv) => (tycs, substvar lv)) table},
	 po,lty,tycs)
      | cpo po = po

    fun cdcon (s,Access.EXN(Access.LVAR lv),lty) =
	(s, Access.EXN(Access.LVAR(substvar lv)), lty)
      | cdcon dc = dc

    fun zip ([],[]) = []
      | zip (x::xs,y::ys) = (x,y)::(zip(xs,ys))
      | zip _ = bug "bad zip"

    (* F.APP inlining (if any)
     * `ifs' is the set of function we are currently inlining
     * `f' is the function, `vs' its arguments.
     * return either (NONE, ifs) if inlining cannot be done or
     * (SOME lexp, nifs) where `lexp' is the expansion of APP(f,vs) and
     * `nifs' is the new set of functions we are currently inlining.
     *)
    fun inline ifs (f,vs) =
	case ((val2sval m f) handle x => raise x)
	 of Fun(g,body,args,{inline,...},od) =>
	    (ASSERT(used g, "used "^(C.LVarString g));
	     if d <> od then (NONE, ifs)
	     else if ((C.usenb(C.get g))handle x => raise x) = 1 andalso not(S.member ifs g) then
							 
		 (* simple inlining:  we should copy the body and then
		  * kill the function, but instead we just move the body
		  * and kill only the function name.  This inlining strategy
		  * looks inoffensive enough, but still requires some care:
		  * see comments at the begining of this file and in cfun *)
		 (inlineWitness := true;
		  C.unuse (fn _ => ()) true g;
		  ASSERT(not (used g), "killed");
		  (SOME(F.LET(map #1 args, F.RET vs, body), od), ifs))
		 
	     (* aggressive inlining (but hopefully safe).  We allow
	      * inlining for mutually recursive functions (isrec)
	      * despite the potential risk.  The reason is that it can
	      * happen that a wrapper (that should be inlined) has to be made
	      * mutually recursive with its main function.  On another hand,
	      * self recursion (C.recursive) is too dangerous to be inlined
	      * except for loop unrolling *)
	     else if (inline = F.IH_ALWAYS andalso not(S.member ifs g)) orelse
		 (inline = F.IH_UNROLL andalso (S.member ifs g)) then
		 let val nle =
			 C.copylexp M.empty (F.LET(map #1 args, F.RET vs, body))
		 in
		     inlineWitness := true;
		     (*  say ("\nInlining "^(C.LVarString g)); *)
		     (app (unuseval (undertake m)) vs) handle x => raise x;
		     (C.unuse (undertake m) true g) handle x => raise x;
		     (SOME(nle, od),
		      (* gross hack: to prevent further unrolling,
		       * I pretend that the rest is not inside the body *)
		      if inline = F.IH_UNROLL then S.rmv(g, ifs) else S.add(g, ifs))
		 end
	     else (NONE, ifs))
	  | sv => (NONE, ifs)
in
    case le
     of F.RET vs => cont(m, F.RET(map substval vs) handle x => raise x)

      | F.LET (lvs,le,body) =>
	let fun clet () =
		loop m le
		     (fn (m,F.RET vs) =>
		      let fun simplesubst ((lv,v),m) =
			      let val sv = (val2sval m v) handle x => raise x
			      in substitute(m, lv, sv, sval2val sv)
			      end
			  val nm = (foldl simplesubst m (zip(lvs, vs)))
		      in loop nm body cont
		      end
		       | (m,nle) =>
			 let val nm = (foldl (fn (lv,m) =>
					      addbind(m, lv, Var(lv, NONE)))
					     m lvs)
			 in case loop nm body cont
			     of F.RET vs => if vs = (map F.VAR lvs) then nle
					    else F.LET(lvs, nle, F.RET vs)
			      | nbody => F.LET(lvs, nle, nbody)
			 end)
	in case le
	    of F.BRANCH (po,vs,le1,le2) =>
	       (* this is a hack originally meant to cleanup the BRANCH mess
		* introduced in flintnm (where each branch returns just true or
		* false which is generally only used as input to a SWITCH).
		* The present code does slightly more than clean up this case *)
 	       let fun known (F.RECORD(_,_,_,le)) = known le
		     | known (F.CON(_,_,_,v,F.RET[F.VAR v'])) = (v = v')
		     | known (F.RET[F.VAR v]) = false
		     | known (F.RET[_]) = true
		     | known _ = false
		   fun cassoc (lv,v,body,wrap) =
 		       if lv = v andalso ((C.usenb(C.get lv)) handle x=> raise x) = 1 andalso
			   known le1 andalso known le2 then
			   (* here I should also check that le1 != le2 *)
 			   let val nle1 = F.LET([lv], le1, body)
 			       val nlv = cplv lv
			       val _ = C.new NONE nlv
 			       val body2 = C.copylexp (M.add(M.empty, lv, nlv))
						      body
 			       val nle2 = F.LET([nlv], le2, body2)
 			   in
 			       loop m (wrap(F.BRANCH(po, vs, nle1, nle2))) cont
 			   end
 		       else
 			   clet()
 	       in case (lvs,body)
 		   of ([lv],le as F.SWITCH(F.VAR v,_,_,NONE)) =>
 		      cassoc(lv, v, le, OU.id)
 		    | ([lv],F.LET(lvs,le as F.SWITCH(F.VAR v,_,_,NONE),rest)) =>
 		      cassoc(lv, v, le, fn le => F.LET(lvs,le,rest))
 		    | _ => clet()
 	       end
	     | _ => clet()
	end

      | F.FIX (fs,le) =>
	let (* register dump bindings *)
	    val m = foldl (fn (fdec as (_,f,_,_),m) =>
			   addbind(m, f, Var(f,NONE)))
			  m fs

	    (* The actual function contraction *)
	    fun cfun (m,[]:F.fundec list,acc) = acc
	      | cfun (m,fdec as ({inline,cconv,known,isrec},f,args,body)::fs,acc) =
		if used f then
		    let (*  val _ = say ("\nEntering "^(C.LVarString f)) *)
			val oldWitness =
			    (!inlineWitness before inlineWitness := false)
			(* make up the bindings for args inside the body *)
			fun addnobind ((lv,lty),m) =
			    addbind(m, lv, Var(lv, SOME lty))
			val nm = foldl addnobind m args
			(* contract the body and create the resulting fundec *)
			val nbody = cexp cfg (S.add(f, ifs)) nm body #2
			(* if inlining took place, the body might be completely
			 * changed (read: bigger), so we have to reset the
			 * `inline' bit *)
			val nfk = {isrec=isrec, cconv=cconv,
				   known=known orelse not(C.escaping(C.get f))handle x => raise x,
				   inline=if !inlineWitness
					  then F.IH_SAFE
					  else (inline before
						inlineWitness := oldWitness)}
			(* update the binding in the map.  This step is not
			 * not just a mere optimization but is necessary
			 * because if we don't do it and the function
			 * gets inlined afterwards, the counts will reflect the
			 * new contracted code while we'll be working on the
			 * the old uncontracted code *)
			val nm = addbind(m, f, Fun(f, nbody, args, nfk, od))
		    in cfun(nm, fs, (nfk, f, args, nbody)::acc)
			   (*  before say ("\nExiting "^(C.LVarString f)) *)
		    end
		else cfun(m, fs, acc)

	    (* check for eta redex *)
	    fun ceta (fdec as (fk,f,args,F.APP(g,vs)):F.fundec,(m,fs,hs)) =
		if vs = (map (F.VAR o #1) args) andalso
		    (* don't forget to check that g is not one of the args
		     * and not f itself either *)
		    (List.find (fn v => v = g) (F.VAR f::vs)) = NONE
		then
		    let val svg = val2sval m g
			val g = case sval2val svg
				 of F.VAR g => g
				  | v => bugval("not a variable", v)
		    (* NOTE: we don't want to turn a known function into an
		     * escaping one.  It's dangerous for optimisations based
		     * on known functions (elimination of dead args, f.ex)
		     * and could generate cases where call>use in collect *)
		    in if not (((C.escaping(C.get f))handle x => raise x) andalso not (C.escaping(C.get g))handle x => raise x)
		       then let
			   (* if an earlier function h has been eta-reduced
			    * to f, we have to be careful to update its
			    * binding to not refer to f any more since f
			    * will disappear *)
			   val nm = foldl (fn (h,m) =>
					   if sval2val(lookup m h) = F.VAR f
					   then addbind(m, h, svg) else m)
					  m hs
		       in
			   (* I could almost reuse `substitute' but the
			    * unuse in substitute assumes the val is escaping *)
			   C.transfer(f, g);
			   C.unuse (undertake m) true g;
			   (addbind(m, f, svg), fs, f::hs)
		       end
		       (* the default case could ensure the inline *)
		       else (m, fdec::fs, hs)
		    end
		else (m, fdec::fs, hs)
	      | ceta (fdec,(m,fs,hs)) = (m,fdec::fs,hs)

	    (* drop constant arguments if possible *)
	    fun cstargs (f as ({inline=F.IH_ALWAYS,...},_,_,_):F.fundec) = f
	      | cstargs (f as (fk,g,args,body):F.fundec) =
		let val actuals = (C.actuals (C.get g)) handle x => raise x
		    val cst =
			ListPair.map
			    (fn (NONE,_) => false
			      | (SOME v,(a,_)) =>
				((case substval v
				  of F.VAR lv =>
				     if used a andalso used lv then
					 (C.use NONE (C.get lv); true)
				     else false
				   | _ => false)
				    handle M.IntmapF => false))
			    (actuals, args)
		(* if all args are used, there's nothing we can do *)
		in if List.all not cst then f else
		    let fun newarg lv =
			    let val nlv = cplv lv in C.new NONE nlv; nlv end
			fun filter xs = OU.filter(cst, xs)
			(* construct the new arg list *)
			val nargs = ListPair.map
					(fn ((a,t),true) => (newarg a,t)
					  | ((a,t),false) => (a,t))
					(args, cst)
			(* construct the new body *)
			val nbody =
			    F.LET(map #1 (filter args),
				  F.RET(map O.valOf (filter actuals)),
				  body)
		    in (fk, g, nargs, nbody)
		    end
		end

	    (* add wrapper for various purposes *)
	    fun wrap (f as ({inline=F.IH_ALWAYS,...},_,_,_):F.fundec,fs) = f::fs
	      | wrap (f as (fk as {isrec,...},g,args,body):F.fundec,fs) =
		let fun dropargs filter =
			let val (nfk,nfk') = OU.fk_wrap(fk, O.map #1 isrec)
			    val args' = filter args
			    val ng = cplv g
			    val nargs = map (fn (v,t) => (cplv v, t)) args
			    val nargs' = map #1 (filter nargs)
			    val appargs = (map F.VAR nargs')
			    val nf = (nfk, g, nargs, F.APP(F.VAR ng, appargs))
			    val nf' = (nfk', ng, args', body)

			    val ngi = C.new (SOME(map #1 args')) ng
			    val nargsi = map ((C.new NONE) o #1) nargs
			in
			    C.use (SOME appargs) ngi;
			    app (C.use NONE) nargsi;
			    nf'::nf::fs
			end
		    val used = map (used o #1) args
		in
		    (* if some args are not used, let's drop them *)
		    if not (List.all OU.id used) then
			dropargs (fn xs => OU.filter(used, xs))

		    (* eta-split: add a wrapper for escaping uses *)
		    else
			let val gi = C.get g
			in if ((C.escaping gi)handle x => raise x) andalso ((C.called gi)handle x => raise x) then
			    (* like dropargs but keeping all args *)
			    dropargs OU.id

			   else f::fs
			end
		end

	    (* junk unused funs *)
	    val fs = List.filter (used o #2) fs

	    (* redirect cst args to their source value *)
	    val fs = map cstargs fs

	    (* add various wrappers *)
	    val fs = foldl wrap [] fs

	    (* register the new bindings (uncontracted for now) *)
	    val nm = foldl (fn (fdec as (fk,f,args,body),m) =>
			    addbind(m, f, Fun(f, body, args, fk, od)))
			   m fs
	    (* check for eta redexes *)
	    val (nm,fs,_) = foldl ceta (nm,[],[]) fs

	    (* move the inlinable functions to the end of the list *)
	    val (f1s,f2s) =
		List.partition (fn ({inline=F.IH_ALWAYS,...},_,_,_) => true
				 | _ => false) fs
	    val fs = f2s @ f1s

	    (* contract the main body *)
	    val nle = loop nm le cont
	    (* contract the functions *)
	    val fs = cfun(nm, fs, [])
	    (* junk newly unused funs *)
	    val fs = List.filter (used o #2) fs
	in
	    case fs
	     of [] => nle
	      | [f1 as ({isrec=NONE,...},_,_,_),f2] =>
		(* gross hack: `wrap' might have added a second
		 * non-recursive function.  we need to split them into
		 * 2 FIXes.  This is _very_ ad-hoc *)
		F.FIX([f2], F.FIX([f1], nle))
	      | _ => F.FIX(fs, nle)
	end
	    
      | F.APP (f,vs) =>
	let val nvs = ((map substval vs) handle x => raise x)
	in case inline ifs (f, nvs)
	    of (SOME(le,od),nifs) => cexp (d,od) ifs m le cont
	     | (NONE,_) => cont(m,F.APP((substval f) handle x => raise x, nvs))
	end
	    
      | F.TFN ((f,args,body),le) =>
	if used f then
	    let val nbody = cexp (DI.next d, DI.next od) ifs m body #2
		val nm = addbind(m, f, TFun(f, nbody, args, od))
		val nle = loop nm le cont
	    in
		if used f then F.TFN((f, args, nbody), nle) else nle
	    end
	else loop m le cont

      | F.TAPP(f,tycs) =>
	cont(m, F.TAPP((substval f) handle x => raise x, tycs))

      | F.SWITCH (v,ac,arms,def) =>
	(case ((val2sval m v) handle x => raise x)
	  of sv as Con (lvc,v,dc1,tycs1) =>
	     let fun killle le = ((#1 (C.unuselexp (undertake m))) le) handle x => raise x
		 fun kill lv le =
		     ((#1 (C.unuselexp (undertake (addbind(m,lv,Var(lv,NONE)))))) le) handle x => raise x
		 fun killarm (F.DATAcon(_,_,lv),le) = kill lv le
		   | killarm _ = buglexp("bad arm in switch(con)", le)

		 fun carm ((F.DATAcon(dc2,tycs2,lv),le)::tl) =
		     (* sometimes lty1 <> lty2 :-( so this doesn't work:
		      *  FU.dcon_eq(dc1, dc2) andalso tycs_eq(tycs1,tycs2) *)
		     if #2 dc1 = #2 (cdcon dc2) then
			 (map killarm tl; (* kill the rest *)
			  O.map killle def; (* and the default case *)
			  loop (substitute(m, lv, val2sval m v, F.VAR lvc))
			       le cont)
		     else
			 (* kill this arm and continue with the rest *)
			 (kill lv le; carm tl)
		   | carm [] = loop m (O.valOf def) cont
		   | carm _ = buglexp("unexpected arm in switch(con,...)", le)
	     in carm arms
	     end

	   | sv as Val v =>
	     let fun kill le = ((#1 (C.unuselexp (undertake m))) le) handle x => raise x
		 fun carm ((con,le)::tl) =
		     if eqConV(con, v) then
			  (map (kill o #2) tl;
			   O.map kill def;
			   loop m le cont)
		     else (kill le; carm tl)
		   | carm [] = loop m (O.valOf def) cont
	     in carm arms
	     end

	   | sv as (Var{1=lvc,...} | Select{1=lvc,...} | Decon{1=lvc, ...}
	     | (* will probably never happen *) Record{1=lvc,...}) =>
	     (case (arms,def)
	       of ([(F.DATAcon(dc,tycs,lv),le)],NONE) =>
		  (* this is a mere DECON, so we can push the let binding
		   * (hidden in cont) inside and maybe even drop the DECON *)
		  let val ndc = cdcon dc
		      val nm = addbind(m, lv, Decon(lv, F.VAR lvc, ndc, tycs))
		      (* see below *)
		      val nm = addbind(nm, lvc, Con(lvc, F.VAR lv, ndc, tycs))
		      val nle = loop nm le cont
		      val nv = sval2val sv
		  in
		      if used lv then
			  F.SWITCH(nv,ac,[(F.DATAcon(ndc,tycs,lv),nle)],NONE)
		      else (unuseval (undertake m) nv; nle)
		  end
		| (([(_,le)],NONE) | ([],SOME le)) =>
		  (* This should never happen, but we can optimize it away *)
		  (unuseval (undertake m) (sval2val sv); loop m le cont)
		| _ =>
		  let fun carm (F.DATAcon(dc,tycs,lv),le) =
			  let val ndc = cdcon dc
			      val nm = addbind(m, lv,
					       Decon(lv, F.VAR lvc, ndc, tycs))
			      (* we can rebind lv to a more precise value
			       * !!BEWARE!!  This rebinding is misleading:
			       * - it gives the impression that `lvc' is built
			       *   from`lv' although the reverse is true:
			       *   if `lvc' is undertaken, `lv's count should
			       *   *not* be updated!
			       *   Luckily, `lvc' will not become dead while
			       *   rebound to Con(lv) because it's used by the
			       *   SWITCH. All in all, it works fine, but it's
			       *   not as straightforward as it seems.
			       * - it seems to be a good idea, but it can hide
			       *   other opt-opportunities since it hides the
			       *   previous binding. *)
			      val nm = addbind(nm, lvc,
					       Con(lvc, F.VAR lv, ndc, tycs))
			  in (F.DATAcon(ndc, tycs, lv), loop nm le #2)
			  end
			| carm (con,le) = (con, loop m le #2)
		      val narms = map carm arms
		      val ndef = Option.map (fn le => loop m le #2) def
		  in cont(m, F.SWITCH(sval2val sv, ac, narms, ndef))
		  end)

	   | sv as (Fun _ | TFun _) =>
	     bugval("unexpected switch arg", sval2val sv))

      | F.CON (dc1,tycs1,v,lv,le) =>
	if used lv then
	    let val ndc = cdcon dc1
		fun ccon sv =
		    let val nv = sval2val sv
			val nm = addbind(m, lv, Con(lv, nv, ndc, tycs1))
			val nle = loop nm le cont
		    in if used lv then F.CON(ndc, tycs1, nv, lv, nle) else nle
		    end
	    in case ((val2sval m v) handle x => raise x)
		of sv as (Decon (lvd,vc,dc2,tycs2)) =>
		   if FU.dcon_eq(dc1, dc2) andalso tycs_eq(tycs1,tycs2) then
		       let val sv = (val2sval m vc) handle x => raise x
		       in loop (substitute(m, lv, sv, F.VAR lvd)) le cont
		       end
		   else ccon sv
		 | sv => ccon sv
	    end
	else loop m le cont

      | F.RECORD (rk,vs,lv,le) =>
	(* g: check whether the record already exists *)
	if used lv then
	    let fun g (n,Select(_,v1,i)::ss) =
		    if n = i then
			(case ss
			  of Select(_,v2,_)::_ =>
			     if v1 = v2 then g(n+1, ss) else NONE
			   | [] => 
			     (case sval2lty (val2sval m v1)
			       of SOME lty =>
				  let val ltd = case rk
						 of F.RK_STRUCT => LT.ltd_str
						  | F.RK_TUPLE _ => LT.ltd_tuple
						  | _ => buglexp("bogus rk",le)
				  in if length(ltd lty) = n+1
				     then SOME v1 else NONE
				  end
				| _ => NONE) (* sad case *)
			   | _ => NONE)
		    else NONE
		  | g _ = NONE
		val svs = ((map (val2sval m) vs) handle x => raise x)
	    in case g (0,svs)
		of SOME v => 
		   let val sv = (val2sval m v) handle x => raise x
		   in loop (substitute(m, lv, sv, F.INT 0)) le cont
			   before app (unuseval (undertake m)) vs
		   end
		 | _ => 
		   let val nvs = map sval2val svs
		       val nm = addbind(m, lv, Record(lv, nvs))
		       val nle = loop nm le cont
		   in if used lv then F.RECORD(rk, nvs, lv, nle) else nle
		   end
	    end
	else loop m le cont

      | F.SELECT (v,i,lv,le) =>
	if used lv then
	    (case ((val2sval m v) handle x => raise x)
	      of Record (lvr,vs) =>
		 let val sv = (val2sval m (List.nth(vs, i))) handle x => raise x
		 in loop (substitute(m, lv, sv, F.VAR lvr)) le cont
		 end
	       | sv =>
		 let val nv = sval2val sv
		     val nm = addbind (m, lv, Select(lv, nv, i))
		     val nle = loop nm le cont
		 in if used lv then F.SELECT(nv, i, lv, nle) else nle
		 end)
	else loop m le cont
		     
      | F.RAISE (v,ltys) =>
	cont(m, F.RAISE((substval v) handle x => raise x, ltys))

      | F.HANDLE (le,v) =>
	cont(m, F.HANDLE(loop m le #2, (substval v) handle x => raise x))

      | F.BRANCH (po,vs,le1,le2) =>
	let val nvs = ((map substval vs) handle x => raise x)
	    val npo = cpo po
	    val nle1 = loop m le1 #2
	    val nle2 = loop m le2 #2
	in cont(m, F.BRANCH(npo, nvs, nle1, nle2))
	end

      | F.PRIMOP (po,vs,lv,le) =>
	let val impure = impurePO po
	in if impure orelse used lv then
	    let val nvs = ((map substval vs) handle x => raise x)
		val npo = cpo po
		val nm = addbind(m, lv, Var(lv,NONE))
		val nle = loop nm le cont
	    in
		if impure orelse used lv
		then F.PRIMOP(npo, nvs, lv, nle)
		else nle
	    end
	   else loop m le cont
	end
end
		 
in
    (*  C.collect fdec; *)
    case cexp (DI.top,DI.top) S.empty
	      M.empty (F.FIX([fdec], F.RET[F.VAR f])) #2
     of F.FIX([fdec], F.RET[F.VAR f]) => fdec
      | fdec => bug "invalid return fundec"
end
	    
end
end
