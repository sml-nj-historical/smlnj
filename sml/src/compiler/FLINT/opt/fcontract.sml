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
 * - dropping of arguments
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
 * - elimination of dead vars in let (subsumes what lcontract does)
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

(* cfg: is used for deBruijn renumbering when inlining at different depths
 * ifs (inlined functions): records which functions we're currently inlining
 *     in order to detect loops
 * m: is a map lvars to their defining expressions (svals) *)
fun cexp (cfg as (d,od)) ifs m le = let

    val loop = cexp cfg ifs

    fun used lv = C.usenb lv > 0
    
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
	((lookup m ov) handle x => (PP.printSval(F.VAR ov); raise x))
      | val2sval m v = Val v

    fun bugsv (msg,sv) = bugval(msg, sval2val sv)

    fun subst m ov = sval2val (lookup m ov)
    val substval = sval2val o (val2sval m)
    fun substvar lv =
	case substval(F.VAR lv)
	 of F.VAR lv => lv
	  | v => bugval ("unexpected val", v)

    fun unuseval f (F.VAR lv) = C.unuse f false lv
      | unuseval f _ = ()

    (* called when a variable becomes dead.
     * it simply adjusts the use-counts *)
    fun undertake m lv =
	let val undertake = undertake m
	in case lookup m lv
	    of Var {1=nlv,...}	 => ASSERT(nlv = lv, "nlv = lv")
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
		(say "\nUnable to undertake "; PP.printSval(F.VAR lv))
		     | x =>
		       (say "\nwhile undertaking "; PP.printSval(F.VAR lv); raise x)

    fun addbind (m,lv,sv) = M.add(m, lv, sv)

    (* substitute a value sv for a variable lv and unuse value v.
     * This doesn't quite work for eta-redex since the `use' we have
     * to remove in that case is a non-escaping use, whereas this code
     * assumes that we're getting rid of an escaping use *)
    fun substitute (m, lv1, sv, v) =
	(case sval2val sv of F.VAR lv2 => C.transfer(lv1,lv2) | v2 => ();
	 unuseval (undertake m) v;
	 addbind(m, lv1, sv)) handle x =>
	     (say "\nwhile substituting ";
	      PP.printSval (F.VAR lv1);
	      say " for ";
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

    fun isrec (F.FK_FCT | F.FK_FUN{isrec=NONE,...}) = false
      | isrec _ = true

    fun inlinable F.FK_FCT = false
      | inlinable (F.FK_FUN{inline,...}) = inline

    (* F.APP inlining (if any)
     * `ifs' is the set of function we are currently inlining
     * `f' is the function, `vs' its arguments.
     * return either (NONE, ifs) if inlining cannot be done or
     * (SOME lexp, nifs) where `lexp' is the expansion of APP(f,vs) and
     * `nifs' is the new set of functions we are currently inlining.
     *)
    fun inline ifs (f,vs) =
	case ((val2sval m f) handle x => raise x)
	 of Fun(g,body,args,fk,od) =>
	    (ASSERT(C.usenb g > 0, "C.usenb g > 0");
	     (* if a function is mutually recursive with one of the
	      * functions inside which we are, inlining it will turn
	      * extrnal uses in internal ones.  The 'body move' optimization
	      * used below cannot be used in such a case *)
	     if C.usenb g = 1 andalso od = d andalso not (isrec fk)
							 
	     (* simple inlining:  we should copy the body and then
	      * kill the function, but instead we just move the body
	      * and kill only the function name.  This inlining strategy
	      * looks inoffensive enough, but still requires some care:
	      * see comments at the begining of this file and in cfun *)
	     then (C.unuse (fn _ => ()) true g; ASSERT(not (used g), "killed");
		   (SOME(F.LET(map #1 args, F.RET vs, body), od), ifs))
		 
	     (* aggressive inlining (but hopefully safe).  We allow
	      * inlining for mutually recursive functions (isrec)
	      * despite the potential risk.  The reason is that it can
	      * happen that a wrapper (that should be inlined) has to be made
	      * mutually recursive with its main function.  On another hand,
	      * self recursion (C.recursive) is too dangerous to be inlined
	      * except for loop unrolling which we don't support yet *)
	     else if ((inlinable fk orelse
		       (C.usenb g = 1 andalso not (C.recursive g)))
			  andalso od = d andalso not(S.member ifs g)) then
		 let val nle =
			 FU.copy M.empty (F.LET(map #1 args, F.RET vs, body))
		     val _ = if C.recursive g then
			 (say "\n inlining recursive function ";
			  PP.printSval (F.VAR g)) else ()
		 in C.uselexp nle;
		     app (unuseval (undertake m)) vs;
		     (* FIXME: this `unuse' can lead to bogus counts if we
		      * currently are in a function mutually recursive with g *)
		      if isrec fk then () else C.unuse (undertake m) true g;
		     (SOME(nle, od), S.add(g, ifs))
		 end
	     else (NONE, ifs))
	  | sv => (NONE, ifs)
in
    case le
     of F.RET vs => F.RET((map substval vs) handle x => raise x)

      | F.LET (lvs,le,body) =>
	let fun cassoc le = F.LET(lvs, le, body)
	    (* default behavior *)
	    fun clet () =
		let val nle = loop m le
		    val nm = foldl (fn (lv,m) => addbind(m, lv, Var(lv, NONE)))
				   m lvs
		in case loop nm body
		    of F.RET vs => if vs = (map F.VAR lvs) then nle
				   else F.LET(lvs, nle, F.RET vs)
		     | nbody => F.LET(lvs, nle, nbody)
		end
	    val lopm = loop m
	in case le
	    (* apply let associativity  *)
	    of F.LET(lvs1,le',le) => lopm(F.LET(lvs1, le', cassoc le))
	     | F.FIX(fdecs,le) => lopm(F.FIX(fdecs, cassoc le))
	     | F.TFN(tfdec,le) => lopm(F.TFN(tfdec, cassoc le))
	     | F.CON(dc,tycs,v,lv,le) => lopm(F.CON(dc, tycs, v, lv, cassoc le))
	     | F.RECORD(rk,vs,lv,le) => lopm(F.RECORD(rk, vs, lv, cassoc le))
	     | F.SELECT(v,i,lv,le) => lopm(F.SELECT(v, i, lv, cassoc le))
	     | F.PRIMOP(po,vs,lv,le) => lopm(F.PRIMOP(po, vs, lv, cassoc le))
	     (* this is a hack originally meant to cleanup the BRANCH mess
	      * introduced in flintnm (where each branch returns just true or
	      * false which is generally only used as input to a SWITCH).
	      * The present code does slightly more than clean up this case *)
 	     | F.BRANCH (po,vs,le1,le2) =>
 	       let fun known (F.RECORD(_,_,_,le)) = known le
		     | known (F.CON(_,_,_,v,F.RET[F.VAR v'])) = (v = v')
		     | known (F.RET[F.VAR v]) = false
		     | known (F.RET[_]) = true
		     | known _ = false
		   fun cassoc (lv,v,body) wrap =
 		       if lv = v andalso C.usenb lv = 1 andalso
			   known le1 andalso known le2 then
			   (* here I should also check that le1 != le2 *)
 			   let val nle1 = F.LET([lv], le1, body)
 			       val nlv = cplv lv
 			       val body2 = FU.copy (M.add(M.empty,lv,nlv)) body
 			       val nle2 = F.LET([nlv], le2, body2)
 			   in C.new false nlv; C.uselexp body2;
 			       lopm(wrap(F.BRANCH(po, vs, nle1, nle2)))
 			   end
 		       else
 			   clet()
 	       in case (lvs,body)
 		   of ([lv],le as F.SWITCH(F.VAR v,_,_,NONE)) =>
 		      cassoc(lv, v, le) (fn x => x)
 		    | ([lv],F.LET(lvs,le as F.SWITCH(F.VAR v,_,_,NONE),rest)) =>
 		      cassoc(lv, v, le) (fn le => F.LET(lvs,le,rest))
 		    | _ => clet()
 	       end
	     | F.RET vs =>
	       (let fun simplesubst ((lv,v),m) =
			let val sv = (val2sval m v) handle x => raise x
			in substitute(m, lv, sv, sval2val sv)
			end
	       in loop (foldl simplesubst m (ListPair.zip(lvs, vs))) body
	       end handle x => raise x)
	     | F.APP(f,vs) =>
	       (case inline ifs (f, vs)
		 of (SOME(le,od),ifs) => cexp (d,od) ifs m (F.LET(lvs, le, body))
		  | (NONE,_) => clet())
	     | (F.TAPP _ | F.SWITCH _ | F.RAISE _ | F.HANDLE _) =>
	       clet()
	end
	
      | F.FIX (fs,le) =>
	let fun cfun (m,[]:F.fundec list,acc) = acc
	      | cfun (m,fdec as (fk,f,args,body)::fs,acc) =
		if used f then
		    let (* make up the bindings for args inside the body *)
			fun addnobind ((lv,lty),m) =
			    addbind(m, lv, Var(lv, SOME lty))
			val nm = foldl addnobind m args
			(* contract the body and create the resulting fundec *)
			val nbody = C.inside f (fn()=> loop nm body)
			(* fixup the fkind info with new data.
			 * C.recursive only tells us if a fun is self-recursive
			 * but doesn't deal with mutual recursion.
			 * Also the `inline' bit has to be turned off because
			 * it applied to the function before contraction
			 * but might not apply to its new form (inlining might
			 * have increased its size substantially or made it
			 * recursive in a different way which could make further
			 * inlining even dangerous) *)
			val nfk =
			    case fk of F.FK_FCT => fk
			      | F.FK_FUN {isrec,fixed,known,inline} =>
				let val nisrec = if isSome isrec andalso
					            null fs andalso
						    null acc andalso
						    not(C.recursive f)
						 then NONE else isrec
				    val nknown = known orelse not(C.escaping f)
				in F.FK_FUN{isrec=nisrec, fixed=fixed,
					    inline=false, known=nknown}
				end
			(* update the binding in the map.  This step is not
			 * not just a mere optimization but is necessary
			 * because if we don't do it and the function
			 * gets inlined afterwards, the counts will reflect the
			 * new contracted code while we'll be working on the
			 * the old uncontracted code *)
			val nm = addbind(m, f, Fun(f, nbody, args, nfk, od))
		    in cfun(nm, fs, (nfk, f, args, nbody)::acc)
		    end
		else cfun(m, fs, acc)

	    (* check for eta redex *)
	    fun ceta ((fk,f,args,F.APP(g,vs)):F.fundec,(m,hs)) =
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
		    in if not (C.escaping f andalso
			       not (C.escaping g))
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
			   (* if g is one of the members of the FIX, f might
			    * appear in its body, so we don't know what parts
			    * of the counts of f should be counted as inside
			    * g and what parts should be counted as outside
			    * so we take the conservative approach of counting
			    * them in both *)
			   if isSome(List.find (fn (_,f,_,_) => f = g) fs)
			   then C.inside g (fn()=> C.addto(f,g)) else ();
			   C.transfer(f,g); C.unuse (undertake nm) true g;
			   (addbind(nm, f, svg),f::hs)
		       end
		       (* the default case could ensure the inline *)
		       else (m, hs)
		    end
		else (m, hs)
	      | ceta (_,(m,hs)) = (m, hs)

	    (* add droparg wrapper if useful *)
	    fun dropargs (f as (fk,g,args,body):F.fundec,fs) =
		case fk
		 of F.FK_FCT => f::fs (* we can't make inlinable fcts *)
		  | F.FK_FUN{inline=true,...} => f::fs (* no use *)
		  | fk as F.FK_FUN{isrec,...} =>
		    let val used = map (fn (v,t) => (C.usenb v > 0)) args
		    (* if all args are used, there's nothing we can do *)
		    in if List.all OU.id used then f::fs else
			let fun filter xs = OU.filter(used, xs)
			    val ng = cplv g
			    val _ = (C.new true ng; C.use true ng; C.extcounts g)
			    val nargs = map (fn (v,t) => (cplv v, t)) args
			    val _ = app (fn (v,t) =>
					 (C.new false v; C.use false v))
					nargs
			    val appargs = (map (F.VAR o #1) nargs)
			    val (nfk,nfk') = OU.fk_wrap(fk, isrec)
			    val nf = (nfk, g, nargs,
				      F.APP(F.VAR ng, filter appargs))
			    val nf' = (nfk', ng, filter args, body)
			in nf'::nf::fs
			end
		    end

	    (* junk unused funs *)
	    val fs = List.filter (used o #2) fs

	    (* add wrappers to drop unused arguments *)
	    val fs = foldl dropargs [] fs

	    (* register the new bindings (uncontracted for now) *)
	    val nm = foldl (fn (fdec as (fk,f,args,body),m) =>
			    addbind(m, f, Fun(f, body, args, fk, od)))
			   m fs
	    (* check for eta redexes *)
	    val (nm,_) = foldl ceta (nm,[]) fs

	    (* move the inlinable functions to the end of the list *)
	    val (f1s,f2s) =
		List.partition (fn (F.FK_FUN{inline,...},_,_,_) => inline
				 | _ => false) fs
	    val fs = f2s @ f1s

	    (* contract the main body *)
	    val nle = loop nm le
	    (* contract the functions *)
	    val fs = cfun(nm, fs, [])
	    (* junk newly unused funs *)
	    val fs = List.filter (used o #2) fs
	in
	    case fs
	     of [] => nle
	      | [f1 as (F.FK_FUN{isrec=NONE,...},f,args,F.APP _),f2] =>
		(* gross hack: dropargs might have added a second
		 * non-recursive function.  we need to split them into
		 * 2 FIXes.  This is very ad-hoc *)
		F.FIX([f2], F.FIX([f1], nle))
	      | (F.FK_FUN{isrec=NONE,...},f,args,body)::_::_ =>
		bug "gross hack failed"
	      | _ => F.FIX(fs, nle)
	end
	    
      | F.APP (f,vs) =>
	let val nvs = ((map substval vs) handle x => raise x)
	in case inline ifs (f, nvs)
	    of (SOME(le,od),ifs) => cexp (d,od) ifs m le
	     | (NONE,_) => F.APP((substval f) handle x => raise x, nvs)
	end
	    
      | F.TFN ((f,args,body),le) =>
	if used f then
	    let val nbody = cexp (DI.next d, DI.next od) ifs m body
		val nm = addbind(m, f, TFun(f, nbody, args, od))
		val nle = loop nm le
	    in
		if used f then F.TFN((f, args, nbody), nle) else nle
	    end
	else loop m le

      | F.TAPP(f,tycs) => F.TAPP((substval f) handle x => raise x, tycs)

      | F.SWITCH (v,ac,arms,def) =>
	(case ((val2sval m v) handle x => raise x)
	  of sv as (Var{1=lvc,...} | Select{1=lvc,...} | Decon{1=lvc, ...}
	            | (* will probably never happen *) Record{1=lvc,...}) =>
	     let fun carm (F.DATAcon(dc,tycs,lv),le) =
		      let val ndc = cdcon dc
			  val nm = addbind(m, lv, Decon(lv, F.VAR lvc, ndc, tycs))
			  (* we can rebind lv to a more precise value
			   * !!BEWARE!!  This rebinding is misleading:
			   * - it gives the impression that `lvc' is built from
			   *   `lv' although the reverse is true:  if `lvc' is
			   *   undertaken, `lv's count should *not* be updated!
			   *   Luckily, `lvc' will not become dead while rebound
			   *   to Con(lv) because it's used by the SWITCH.
			   *   All in all, it works fine, but it's not as
			   *   straightforward as it seems.
			   * - it seems to be a good idea, but it can hide
			   *   other opt-opportunities since it hides the
			   *   previous binding. *)
			  val nm = addbind(nm, lvc, Con(lvc, F.VAR lv, ndc, tycs))
		      in (F.DATAcon(ndc, tycs, lv), loop nm le)
		      end
		    | carm (con,le) = (con, loop m le)
		  val narms = map carm arms
		  val ndef = Option.map (loop m) def
	     in
		  F.SWITCH(sval2val sv, ac, narms, ndef)
	     end

	   | Con (lvc,v,dc1,tycs1) =>
	     let fun killle le = (#1 (C.unuselexp (undertake m))) le
		 fun kill lv le =
		     (#1 (C.unuselexp (undertake (addbind(m,lv,Var(lv,NONE)))))) le
		 fun killarm (F.DATAcon(_,_,lv),le) = kill lv le
		   | killarm _ = buglexp("bad arm in switch(con)", le)

		 fun carm ((F.DATAcon(dc2,tycs2,lv),le)::tl) =
		     if FU.dcon_eq(dc1, dc2) andalso tycs_eq(tycs1,tycs2) then
			 (map killarm tl; (* kill the rest *)
			  Option.map killle def; (* and the default case *)
			  loop (substitute(m, lv, val2sval m v, F.VAR lvc)) le)
		     else
			 (* kill this arm and continue with the rest *)
			 (kill lv le; carm tl)
		   | carm [] = loop m (Option.valOf def)
		   | carm _ = buglexp("unexpected arm in switch(con,...)", le)
	     in carm arms
	     end

	   | Val v =>
	     let fun kill le = (#1 (C.unuselexp (undertake m))) le
		 fun carm ((con,le)::tl) =
		     if eqConV(con, v) then
			  (map (kill o #2) tl; Option.map kill def; loop m le)
		     else (kill le; carm tl)
		   | carm [] = loop m (Option.valOf def)
	     in carm arms
	     end
	   | sv as (Fun _ | TFun _) =>
	     bugval("unexpected switch arg", sval2val sv))

      | F.CON (dc1,tycs1,v,lv,le) =>
	(* Here we should try to nullify CON(DECON x) => x *)
	if used lv then
	    let val ndc = cdcon dc1
		fun ccon sv =
		    let val nv = sval2val sv
			val nm = addbind(m, lv, Con(lv, nv, ndc, tycs1))
			val nle = loop nm le
		    in if used lv then F.CON(ndc, tycs1, nv, lv, nle) else nle
		    end
	    in case ((val2sval m v) handle x => raise x)
		of sv as (Decon (lvd,vc,dc2,tycs2)) =>
		   if FU.dcon_eq(dc1, dc2) andalso tycs_eq(tycs1,tycs2) then
		       let val sv = (val2sval m vc) handle x => raise x
		       in loop (substitute(m, lv, sv, F.VAR lvd)) le
		       end
		   else ccon sv
		 | sv => ccon sv
	    end
	else loop m le

      | F.RECORD (rk,vs,lv,le) =>
	(* Here I could try to see if I'm reconstructing a preexisting record.
	 * The `lty option' of Var is there just for that purpose *)
	if used lv then
	    (* g: check whether the record already exists *)
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
		   in loop (substitute(m, lv, sv, F.INT 0)) le
	               before app (unuseval (undertake m)) vs
		   end
		 | _ => 
		   let val nvs = map sval2val svs
		       val nm = addbind(m, lv, Record(lv, nvs))
		       val nle = loop nm le
		   in if used lv then F.RECORD(rk, nvs, lv, nle) else nle
		   end
	    end
	else loop m le

      | F.SELECT (v,i,lv,le) =>
	if used lv then
	    case ((val2sval m v) handle x => raise x)
	     of Record (lvr,vs) =>
		let val sv = (val2sval m (List.nth(vs, i))) handle x => raise x
		in loop (substitute(m, lv, sv, F.VAR lvr)) le
		end
	      | sv =>
		let val nv = sval2val sv
		    val nm = addbind (m, lv, Select(lv, nv, i))
		    val nle = loop nm le
		in if used lv then F.SELECT(nv, i, lv, nle) else nle
		end
	else loop m le
		     
      | F.RAISE (v,ltys) => F.RAISE((substval v) handle x => raise x, ltys)

      | F.HANDLE (le,v) => F.HANDLE(loop m le, (substval v) handle x => raise x)

      | F.BRANCH (po,vs,le1,le2) =>
	let val nvs = ((map substval vs) handle x => raise x)
	    val npo = cpo po
	    val nle1 = loop m le1
	    val nle2 = loop m le2
	in F.BRANCH(npo, nvs, nle1, nle2)
	end

      | F.PRIMOP (po,vs,lv,le) =>
	let val impure = impurePO po
	in if impure orelse used lv then
	    let val nvs = ((map substval vs) handle x => raise x)
		val npo = cpo po
		val nm = addbind(m, lv, Var(lv,NONE))
		val nle = loop nm le
	    in
		if impure orelse used lv
		then F.PRIMOP(npo, nvs, lv, nle)
		else nle
	    end
	   else loop m le
	end
end
		 
fun contract (fdec as (_,f,_,_)) =
    (C.collect fdec;
     case cexp (DI.top,DI.top) S.empty M.empty (F.FIX([fdec], F.RET[F.VAR f]))
      of F.FIX([fdec], F.RET[F.VAR f]) => fdec
       | fdec => bug "invalid return fundec")
	    
end
end
