(* copyright 1998 YALE FLINT PROJECT *)
(* monnier@cs.yale.edu *)

(* This module does various FIX-related transformations:
 * - FIXes are split into their strongly-connected components
 * - small non-recursive functions are marked inlinable
 * - curried functions are uncurried
 *)

signature FIXFIX =
sig
    val fixfix : FLINT.prog -> FLINT.prog
end

(* Maybe later:
 * - hoisting of inner functions out of their englobing function
 *   so that the outer function becomes smaller, giving more opportunity
 *   for inlining.
 * - eta expand escaping functions
 * - loop-preheader introduction
 *)

structure FixFix :> FIXFIX =
struct

local
    structure F  = FLINT
    structure S = IntSetF
    structure M = IntmapF
    structure PP = PPFlint
    structure LT = LtyExtern
    structure OU = OptUtils
    structure CTRL = Control.FLINT
in

val say = Control.Print.say
fun bug msg = ErrorMsg.impossible ("FixFix: "^msg)
fun buglexp (msg,le) = (say "\n"; PP.printLexp le; say " "; bug msg)
fun bugval (msg,v) = (say "\n"; PP.printSval v; say " "; bug msg)
fun assert p = if p then () else bug ("assertion failed")

val cplv = LambdaVar.dupLvar

(* to limit the amount of uncurrying *)
val maxargs = CTRL.maxargs

structure SccNode = struct
    type node = LambdaVar.lvar
    val eq = (op =)
    val lt = (op <)
end
structure SCC = SCCUtilFun (structure Node = SccNode)

(* fexp: (intset * lexp) -> (int * intset * lexp)
 * The three subparts returned are:
 * - the size of lexp
 * - the set of freevariables of lexp (plus the ones passed as arguments
 *   which are assumed to be the freevars of the continuation of lexp)
 * - a new lexp with FIXes rewritten.
 *)
fun fexp (fv,lexp) = let

    fun addv (s,F.VAR lv) = S.add(lv, s)
      | addv (s,_) = s
    fun addvs (s,vs) = foldl (fn (v,s) => addv(s, v)) s vs
    fun rmvs (s,lvs) = foldl S.rmv s lvs

    (* Looks for free vars in the primop descriptor.
     * This is normally unnecessary since these are special vars anyway *)
    fun fpo (fv,(NONE:F.dict option,po,lty,tycs)) = fv
      | fpo (fv,(SOME{default,table},po,lty,tycs)) =
	addvs(addv(fv, F.VAR default), map (F.VAR o #2) table)

    (* Looks for free vars in the primop descriptor.
     * This is normally unnecessary since these are exception vars anyway *)
    fun fdcon (fv,(s,Access.EXN(Access.LVAR lv),lty)) = addv(fv, F.VAR lv)
      | fdcon (fv,_) = fv

    (* recognize the curried essence of a function.
     * - hd:bool identifies the head of the (potentially) curried function
     * - r:bool indicates whether the head was recursive
     * - na:int gives the number of args still allowed *)
    fun curry (hd,r,na) (le as (F.FIX([(fk,f,args,body)], F.RET[F.VAR lv]))) =
	if lv = f andalso #inline fk = F.IH_SAFE then
	    let val fisrec = isSome(#isrec fk)
		val na = na - length args
	    in if na >= 0 andalso (hd orelse r orelse not fisrec) then
		(* recursive functions are only accepted for uncurrying
		 * if they are the head of the function or if the head
		 * is already recursive *)
		let val (funs,body) =
			curry (false, r orelse fisrec, na) body
		in ((fk,f,args)::funs,body)
		end
	       else ([], le)
	    end
	else
	    (* this "never" occurs, but dead-code removal is not bullet-proof *)
	    ([], le)
      | curry first le = ([], le)

    (* do the actual uncurrying *)
    fun uncurry (args as (fk,f,fargs)::_::_,body) =
	let val f' = cplv f	(* the new fun name *)

	    (* find the rtys of the uncurried function *)
	    fun getrtypes (({isrec=SOME(rtys,_),...}:F.fkind,_,_),_) = SOME rtys
	      | getrtypes ((_,_,_),rtys) =
		Option.map (fn [lty] => #2(LT.ltd_fkfun lty)
			     | _ => bug "strange isrec") rtys

	    (* create the new fkinds *)
	    val (nfk,nfk') = OU.fk_wrap(fk, foldl getrtypes NONE args)

	    (* funarg renaming *)
	    fun newargs fargs = map (fn (a,t) => (cplv a,t)) fargs

	    (* create (curried) wrappers to be inlined *)
	    fun recurry ([],args) = F.APP(F.VAR f', map (F.VAR o #1) args)
	      | recurry (({inline,isrec,known,cconv},f,fargs)::rest,args) =
		let val fk = {inline=F.IH_ALWAYS, isrec=NONE,
			      known=known, cconv=cconv}
		    val nfargs = newargs fargs
		    val g = cplv f'
		in F.FIX([(fk, g, nfargs, recurry(rest, args @ nfargs))],
			 F.RET[F.VAR g])
		end

	    (* build the new f fundec *)
	    val nfargs = newargs fargs
	    val nf = (nfk, f, nfargs, recurry(tl args, nfargs))

	    (* make up the body of the uncurried function (creating
	     * dummy wrappers for the intermediate functions that are now
	     * useless).
	     * Intermediate functions that were not marked as recursive
	     * cannot appear in the body, so we don't need to build them.
	     * Note that we can't just rely on dead-code elimination to remove
	     * them because we may not be able to create them correctly with
	     * the limited type information gleaned in this phase. *)
	    fun uncurry' ([],args) = body
	      | uncurry' ((fk,f,fargs)::rest,args) =
		let val le = uncurry'(rest, args @ fargs)
		in case fk
		    of {isrec=SOME _,cconv,known,inline} =>
		       let val nfargs = newargs fargs
			   val fk = {isrec=NONE, inline=F.IH_ALWAYS,
				     known=known, cconv=cconv}
		       in F.FIX([(fk, f, nfargs,
				  recurry(rest, args @ nfargs))],
				le)
		       end
		     | _ => le
		end

	    (* the new f' fundec *)
	    val nfbody' = uncurry'(tl args, fargs)
	    val nf' = (nfk', f', foldr (op @) [] (map #3 args), nfbody')

	in (nf, nf')
	end
      | uncurry (_,body) = bug "uncurrying a non-curried function"

in case lexp
    of F.RET vs => (0, addvs(fv, vs), lexp)
     | F.LET (lvs,le1,le2) =>
       let val (s2,fv,le2) = fexp(fv, le2)
	   val (s1,fv,le1) = fexp(rmvs(fv, lvs), le1)
       in (s1 + s2, fv, F.LET(lvs, le1, le2))
       end
     | F.FIX (fdecs,le) =>
       let val funs = S.make(map #2 fdecs) (* set of funs defined by the FIX *)

	   (* process the main lexp and make it into a dummy function.
	    * The computation of the freevars is a little sloppy since `fv'
	    * includes freevars of the continuation, but the uniqueness
	    * of varnames ensures that S.inter(fv, funs) gives the correct
	    * result nonetheless. *)
	   val (s,fv,le) = fexp(fv, le)
	   val lename = LambdaVar.mkLvar()
	   val m = M.singleton(lename, (S.members(S.inter(fv, funs)), 0,
					{inline=F.IH_SAFE, isrec=NONE,
					 known=true,cconv=F.CC_FCT},
					[], le))

	   (* process each fun *)
	   fun ffun (fdec as (fk,f,args,body):F.fundec,(s,fv,funs,m)) =
	       case curry (true,false,!maxargs) (F.FIX([fdec], F.RET[F.VAR f]))
		of (args as _::_::_,body) => (* curried function *)
		   let val ((fk,f,fargs,fbody),(fk',f',fargs',fbody')) =
			   uncurry(args,body)
		       (* add the wrapper function *)
		       val nm = M.add(m, f, ([f'], 1, fk, fargs, fbody))
		   (* now, retry ffun with the uncurried function *)
		   in ffun((fk', f', fargs', fbody'),
			   (s+1, fv, S.add(f', funs), nm))
		   end
		 | _ =>	(* non-curried function *)
		   let val (fs,ffv,body) = fexp(S.empty, body)
		       val ffv = rmvs(ffv, map #1 args) (* fun's freevars *)
		       val ifv = S.inter(ffv, funs) (* set of rec funs ref'ed *)
		   in
		       (fs + s, S.union(ffv, fv), funs,
			M.add(m, f, (S.members ifv, fs, fk, args, body)))
		   end

	   (* process the functions, collecting them in map m *)
	   val (s,fv,funs,m) = foldl ffun (s, fv, funs, m) fdecs

	   (* find strongly connected components *)
	   val top = SCC.topOrder{root=lename, follow= #1 o (M.lookup m)}

	   (* turns them back into flint code *)
	   fun sccconvert (SCC.SIMPLE f,le) =
	       (* a simple function.  Fix the fk accordingly *)
	       let val (_,s,{isrec,cconv,known,inline},args,body) = M.lookup m f
		   val fk =
		       (* small functions inlining heuristic *)
		       let val inline' =
			       if inline = F.IH_SAFE andalso 
				   s < !CTRL.inlineThreshold then
				   F.IH_ALWAYS
			       else inline
		       in {isrec=NONE, inline=inline',
			   known=known, cconv=cconv}
		       end
	       in F.FIX([(fk, f, args, body)], le)
	       end
	     | sccconvert (SCC.RECURSIVE fs,le) =
	       let fun scfun f =
		       let val (_,s,fk as {isrec,cconv,known,inline},args,le) =
			       M.lookup m f
			   val fk' =
			       (* let's check for unroll opportunities.
				* This heuristic is pretty bad since it doesn't
				* take the number of rec-calls into account *)
			       case (isrec,inline)
				of (SOME(_,(F.LK_LOOP|F.LK_WHILE)),F.IH_SAFE) =>
				   if s < !CTRL.unrollThreshold then
				       {inline=F.IH_UNROLL, isrec=isrec,
					cconv=cconv, known=known}
				   else fk
				 | _ => fk
		       in (fk, f, args, le) end
	       in F.FIX(map scfun fs, le)
	       end
       in
	   case top
	    of (SCC.SIMPLE f)::sccs =>
	       (assert(f = lename);
		(s, S.diff(fv, funs), foldl sccconvert le sccs))
	     | (SCC.RECURSIVE _)::_ => bug "recursive main body in SCC ?!?!?"
	     | [] => bug "SCC going crazy"
       end
     | F.APP (f,args) =>
       (* the cost of a function call depends on the number of args
	* and the size of the continuation (number of free vars).
	* We could also ask Collect whether f is known *)
       (3 + (length args) + (S.size fv), addvs(fv, f::args), lexp)
     | F.TFN ((f,args,body),le) =>
       let val (se,fve,le) = fexp(fv, le)
	   val (sb,fvb,body) = fexp(S.empty, body)
       in (sb + se, S.union(S.rmv(f, fve), fvb), F.TFN((f, args, body), le))
       end
     | F.TAPP (f,args) =>
       (* The cost of TAPP is kinda hard to estimate.  It can be very cheap,
	* and just return a function, or it might do all kinds of wrapping
	* but we have almost no information on which to base our choice.
	* We opted for cheap here, to try to inline them more (they might
	* become cheaper once inlined) *)
       (3, addv(fv, f), lexp)
     | F.SWITCH (v,ac,arms,def) =>
       let fun farm (dcon as F.DATAcon(dc,_,lv),le) =
	       (* the binding might end up costly, but we count it as 1 *)
	       let val (s,fv,le) = fexp(fv,le)
	       in (1+s, fdcon(S.rmv(lv, fv),dc), (dcon, le))
	       end
	     | farm (dc,le) =
	       let val (s,fv,le) = fexp(fv, le) in (s, fv, (dc, le)) end
	   val (s,fv,arms) =
	       foldl (fn ((s1,fv1,arm),(s2,fv2,arms)) =>
		      (s1+s2, S.union(fv1, fv2), arm::arms))
		     (0, fv, []) (map farm arms)
       in case def
	   of NONE => (s, fv, F.SWITCH(v, ac, arms, NONE))
	    | SOME le => let val (sd,fvd,le) = fexp(fv,le)
	      in (s+sd, S.union(fv, fvd), F.SWITCH(v, ac, arms, SOME le))
	      end
       end
     | F.CON (dc,tycs,v,lv,le) =>
       let val (s,fv,le) = fexp(fv, le)
       in (2+s, fdcon(addv(S.rmv(lv, fv), v),dc), F.CON(dc, tycs, v, lv, le))
       end
     | F.RECORD (rk,vs,lv,le) =>
       let val (s,fv,le) = fexp(fv, le)
       in ((length vs)+s, addvs(S.rmv(lv, fv), vs), F.RECORD(rk, vs, lv, le))
       end
     | F.SELECT (v,i,lv,le) =>
       let val (s,fv,le) = fexp(fv, le)
       in (1+s, addv(S.rmv(lv, fv), v), F.SELECT(v,i,lv,le))
       end
     | F.RAISE (v,ltys) => (3, addv(fv, v), lexp)
     | F.HANDLE (le,v) =>
       let val (s,fv,le) = fexp(fv, le)
       in (2+s, addv(fv, v), F.HANDLE(le,v))
       end
     | F.BRANCH (po,vs,le1,le2) =>
       let val (s1,fv1,le1) = fexp(fv,le1)
	   val (s2,fv2,le2) = fexp(fv,le2)
       in (1+s1+s2, fpo(addvs(S.union(fv1, fv2), vs), po),
	   F.BRANCH(po, vs, le1, le2))
       end
     | F.PRIMOP (po,vs,lv,le) =>
       let val (s,fv,le) = fexp(fv, le)
       in (1+s, fpo(addvs(S.rmv(lv, fv), vs),po), F.PRIMOP(po,vs,lv,le))
       end
end

fun fixfix ((fk,f,args,body):F.prog) =
    let val (s,fv,nbody) = fexp(S.empty, body)
	val fv = S.diff(fv, S.make(map #1 args))
    in
	(*  PPFlint.printLexp(F.RET(map F.VAR (S.members fv))); *)
	assert(S.isEmpty(fv));
	(fk, f, args, nbody)
    end

end
end
