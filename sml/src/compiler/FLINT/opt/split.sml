(* copyright 1999 YALE FLINT project *)
(* monnier@cs.yale.edu *)

signature FSPLIT =
sig
    type flint = FLINT.prog
    val split: flint -> flint * flint option
end

structure FSplit :> FSPLIT =
struct

local
    structure F  = FLINT
    structure S  = IntSetF
    structure OU = OptUtils
    structure FU = FlintUtil
    structure LT = LtyDef
    structure PO = PrimOp
    structure PP = PPFlint
in

val say = Control.Print.say
fun bug msg = ErrorMsg.impossible ("FSplit: "^msg)
fun buglexp (msg,le) = (say "\n"; PP.printLexp le; say " "; bug msg)
fun bugval (msg,v) = (say "\n"; PP.printSval v; say " "; bug msg)
fun assert p = if p then () else bug ("assertion failed")

type flint = F.prog
val mklv = LambdaVar.mkLvar
val cplv = LambdaVar.dupLvar

fun addv (s,F.VAR lv) = S.add(lv, s)
  | addv (s,_) = s
fun addvs (s,vs) = foldl (fn (v,s) => addv(s, v)) s vs
fun rmvs (s,lvs) = foldl S.rmv s lvs

(*
fun join (f,args,fdecI as (fkI,fI,argsI,bodyI),fdecE as (fkE,fE,argsE,bodyE)) =
    let val (nfk,_) = OU.fk_wrap(fk, NONE)
	val argsv = map (fn (v,t) => F.VAR v) args
	val nbody =
	    let val tmp = mklv()
	    in F.LET([tmp], F.APP(F.VAR fE, argsv),
		     F.APP(F.VAR fI, (F.VAR tmp)::argsv))
	    end
	val nfdec = (nfk,f,args,nbody)
    in
	SOME(fn e =>
	     F.FIX([fdecE],
		   F.FIX([fdecI],
			 F.FIX([nfdec], e))),
	     F.FIX([fdecI], F.FIX([nfdec], leI)),
	     S.add(fE, rmvs(S.union(fvI, FU.freevars bodyI),
	                    f::(map #1 args))))
    end
*)

fun split (fdec as (fk,f,args,body)) = let
    val {getLty, cleanUp} = Recover.recover (fdec, false)

(*
 * - copy inlinable elements into a second lexp (expI)
 * - make a `lexp -> lexp' wrapper expE that returns the original lexp
 *   with the argument as the last return-lexp
 * - go through expI bottom-up eliminating dead elements and collecting
 *   free variables
 * - return expE and expI along with expI's free variables
 *)
fun sexp lexp =
    case lexp
     (* we can completely move both RET and TAPP to the I part *)
     of F.RET vs =>
	SOME(fn e => e, lexp, addvs(S.empty, vs))
      | F.TAPP (F.VAR tf,tycs) =>
	SOME(fn e => e, lexp, S.singleton tf)

      (* other non-binding lexps result in unsplittable functions *)
      | F.APP (F.VAR f,args) => NONE
      | (F.APP _ | F.TAPP _) => bug "strange (T)APP"
      | (F.SWITCH _ | F.RAISE _ | F.BRANCH _) => NONE

      (* binding-lexps *)
      | (F.LET (_,_,le) | F.FIX (_,le) | F.TFN (_,le) |
	 F.CON (_,_,_,_,le) | F.RECORD (_,_,_,le) | F.SELECT (_,_,_,le) |
	 F.HANDLE (le,_) | F.PRIMOP (_,_,_,le)) =>
	case sexp le
	 of NONE => NONE
	  | SOME (leE,leI,fvI) => let

		fun let1 (lewrap,lv,vs,effect) =
		    let val leE = lewrap o leE
		    in if effect orelse not (S.member fvI lv)
		       then SOME(leE, leI, fvI)
		       else SOME(leE, lewrap leI,
				 addvs(S.rmv(lv, fvI), vs))
		    end

	    in case lexp
		(* Functions definitions fall into the following categories:
		 * - (mutually) recursive:  don't bother
		 * - inlinable:  if exported, copy to leI
		 * - non-inlinable non-recursive:  split recursively *)
		of F.FIX (fs,_) =>
		   let val leE = fn e => F.FIX(fs, leE e)
		   in case fs
		       of [({inline=(F.IH_ALWAYS | F.IH_MAYBE _),...},
			    f,args,body)] =>
			  if not (S.member fvI f)
			  then SOME(leE, leI, fvI)
			  else SOME(leE, F.FIX(fs, leI),
				    rmvs(S.union(fvI, FU.freevars body),
					 f::(map #1 args)))
			| [fdec as (fk as {isrec=NONE,...},f,args,_)] =>
			  (case sfdec fdec
			    of (_, NONE) => SOME(leE, leI, fvI)
			     | (fdecE as (fkE,fE,argsE,bodyE), SOME fdecI) =>
			       let val fdecI as (fkI,fI,argsI,bodyI) =
				       FU.copyfdec fdecI
				   val (nfk,_) = OU.fk_wrap(fk, NONE)
				   val nargs = map (fn (v,t) => (cplv v, t)) args
				   val argsv = map (fn (v,t) => F.VAR v) nargs
				   val nbody =
				       let val tmp = mklv()
				       in F.LET([tmp], F.APP(F.VAR fE, argsv),
						F.APP(F.VAR fI, (F.VAR tmp)::argsv))
				       end
				   val nfdec = (nfk,f,nargs,nbody)
			       in
				   SOME(fn e => F.FIX([fdecE],
						      F.FIX([fdecI],
							    F.FIX([nfdec], e))),
					F.FIX([fdecI], F.FIX([nfdec], leI)),
					S.add(fE, rmvs(S.union(fvI, FU.freevars bodyI),
							 f::(map #1 args))))
			       end)
			| _ => SOME(leE, leI, fvI)
		   end

		 (* TFNs are kinda like FIX except there's no recursion *)
		 | F.TFN (tf,_) =>
		   (* FIXME *)
		   SOME(fn e => F.TFN(tf, leE e), leI, fvI)
		   
		 (* non-side effecting binds are copied to leI if exported *)
		 | F.CON (dc,tycs,v,lv,_) =>
		   let1(fn e => F.CON(dc, tycs, v, lv, e), lv, [v], false)
		 | F.RECORD (rk,vs,lv,_) =>
		   let1(fn e => F.RECORD(rk, vs, lv, e), lv, vs, false)
		 | F.SELECT (v,i,lv,_) =>
		   let1(fn e => F.SELECT(v, i, lv, e), lv, [v], false)
		 | F.PRIMOP (po,vs,lv,_) =>
		   let1(fn e => F.PRIMOP(po,vs,lv,e), lv, vs, PO.effect(#2 po))

		 (* IMPROVEME: lvs should not be restricted to [lv] *)
		 | F.LET (lvs as [lv],body as F.TAPP (v,tycs),_) =>
		   let1(fn e => F.LET(lvs, body, e), lv, [v], false)
		 | F.LET (lvs as [lv],body as F.APP (v,vs),_) =>
		   let1(fn e => F.LET(lvs, body, e), lv, v::vs, true)
		 | F.LET (lvs,body,_) =>
		   SOME(fn e => F.LET(lvs, body, leE e), leI, fvI)

		 | F.HANDLE (_,v) =>
		   SOME(fn e => F.HANDLE(leE e, v), leI, fvI)
		 | _ => bug "second match failed ?!?!"
	    end


and sfdec (fdec as ({cconv=F.CC_FUN _,...},_,_,_)) = (fdec, NONE)
  | sfdec (fdec as (fk as {inline,cconv,known,isrec},f,args,body)) =
    case sexp body
     of NONE => (fdec, NONE)
      | SOME (leE,leI,fvI) =>
	let val fvI = S.members(rmvs(fvI, map #1 args))
	    val fE = cplv f
	    val fI = cplv f
	    val tmp = mklv()
	    val bodyE = leE(F.RECORD(F.RK_STRUCT, map F.VAR fvI,
				     tmp, F.RET[F.VAR tmp]))
	    val argI = mklv()
	    val (_,bodyI) = foldl (fn (lv,(n,le)) =>
				   (n+1, F.SELECT(F.VAR argI, n, lv, le)))
				  (0, leI) fvI
	    val fkI = {inline=F.IH_ALWAYS, cconv=cconv, known=known, isrec=NONE}
	    val argsI = (argI, LT.ltc_str(map (getLty o F.VAR) fvI))::args
	in ((fk, fE, args, bodyE), SOME(fkI, fI, argsI, bodyI))
	end

in case sfdec fdec
    of (fdec,NONE) => (fdec, NONE)
     | (fdecE as (fkE,fE,argsE,bodyE), SOME fdecI) =>
       let val fdecI as (fkI,fI,argsI,bodyI) = FU.copyfdec fdecI
	   val (nfk,_) = OU.fk_wrap(fk, NONE)
	   val nargs = map (fn (v,t) => (cplv v, t)) args
	   val argsv = map (fn (v,t) => F.VAR v) nargs
	   val tmp = mklv()
       in
	   ((fk, f, nargs,
	     F.FIX([fdecE],
		   F.FIX([fdecI],
			 F.LET([tmp], F.APP(F.VAR fE, argsv),
			       F.APP(F.VAR fI, (F.VAR tmp)::argsv))))),
	    NONE)
       end
end

end
end
