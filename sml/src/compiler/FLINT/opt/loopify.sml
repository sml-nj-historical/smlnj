(* copyright 1998 YALE FLINT PROJECT *)
(* monnier@cs.yale.edu *)

signature LOOPIFY =
sig
    val loopify : FLINT.prog -> FLINT.prog
end


structure Loopify =
struct
local
    structure F  = FLINT
    structure C  = Collect
    structure O  = Option
    structure M  = IntmapF
    structure LK = LtyKernel
in

fun bug msg = ErrorMsg.impossible ("Loopify: "^msg)
val cplv = LambdaVar.dupLvar

fun loopify (prog as (progkind,progname,progargs,progbody)) = let

(* m: int intmap	renaming for function calls
 * tf:(int,int) option	the current function (if any) and its tail version
 * le:			you get the idea *)
fun lexp m tf le = let
    val loop = lexp m tf
in case le
    of F.RET _ => le
     | F.LET(lvs,body,le) => F.LET(lvs, lexp m NONE body, loop le)
     | F.FIX(fdecs,le) =>
       let fun cfun (fk:F.fkind as {isrec=SOME(ltys,F.LK_UNKNOWN),cconv,...},
		     f,args,body) =
	       let val fi = C.get f
		   val icallnb = C.icallnb fi
	       (* cpsopt uses the following condition:
		*     escape = 0 andalso !unroll_call > 0
		*    	    andalso (!call - !unroll_call > 1 
		*    		     orelse List.exists (fn t=>t) inv)
		* `escape = 0': I don't quite see the need for it, though it
		*     probably won't change much since etasplit should have
		*     made "everything" known already.
		* `!call - !unroll_call > 1 orelse List.exists (fn t=>t) inv)':
		*     loopification is only useful if there is more than one
		*     external call or if there are loop invariants.
		*     Note that we deal with invariants elsewhere, so it's
		*     not a good reason to loopify here. *)
	       (*** rationale behind the restrictions: ***
		* `icallnb = 0': loopification is pointless and will be
		*     undone by fcontract.
		* `C.callnb fi <= icallnb + 1': if there's only one external
		*     call, loopification will probably (?) not be of much use
		*     and the same benefit would be had by just moving f *)
	       in if icallnb = 0 (* orelse (C.callnb fi <= icallnb + 1) *)
		  (* not a good loop candidate *)
		  then (fk, f, args, loop body)
		  else
		      let val fl = cplv f
			  val ft = cplv f
			  val largs = map (fn(v,t) => (cplv v, t)) args
			  val args' = map (fn(v,t) => (cplv v, t)) args
			  val cconv' =
			      case cconv
			       of (F.CC_FCT | F.CC_FUN(LK.FF_FIXED)) => cconv
				| F.CC_FUN(LK.FF_VAR(f1,f2)) =>
				  F.CC_FUN(LK.FF_VAR(true,f2))
			  val nm = M.add(m, f, fl)
			  val tailloop =
			      F.FIX([({isrec=SOME(ltys, F.LK_WHILE),
				       known=true, inline=F.IH_SAFE,
				       cconv=cconv'}, ft, args,
				      lexp nm (SOME(f,ft)) body)],
				    F.APP(F.VAR ft, map (F.VAR o #1) largs))
		      in (fk, f, args',
			  F.FIX([({isrec=SOME(ltys, F.LK_LOOP),
				   known=true, inline=F.IH_SAFE,
				   cconv=cconv'}, fl, largs,
				  tailloop)],
				F.APP(F.VAR fl, map (F.VAR o #1) args')))
		      end
	       end
	     | cfun (fk,f,args,body) = (fk, f, args, lexp m NONE body)
       in F.FIX(map cfun fdecs, loop le)
       end
     | F.APP(F.VAR f,vs) =>
       (let val fl = M.lookup m f
       in case tf
	   of SOME(f',ft) => if f' = f then F.APP(F.VAR ft, vs)
			     else F.APP(F.VAR fl, vs)
	    | NONE => F.APP(F.VAR fl, vs)
       end handle M.IntmapF => le)
     | F.TFN((f,args,body),le) => F.TFN((f, args, loop body), loop le)
     | F.TAPP(f,tycs) => le
     | F.SWITCH(v,ac,arms,def) =>
       let fun carm (con,le) = (con, loop le)
       in F.SWITCH(v, ac, map carm arms, O.map loop def)
       end
     | F.CON(dc,tycs,v,lv,le) => F.CON(dc, tycs, v, lv, loop le)
     | F.RECORD(rk,vs,lv,le) => F.RECORD(rk, vs, lv, loop le)
     | F.SELECT(v,i,lv,le) => F.SELECT(v, i, lv, loop le)
     | F.RAISE(v,ltys) => le
     | F.HANDLE(le,v) => F.HANDLE(loop le, v)
     | F.BRANCH(po,vs,le1,le2) => F.BRANCH(po, vs, loop le1, loop le2)
     | F.PRIMOP(po,vs,lv,le) => F.PRIMOP(po, vs, lv, loop le)

     | F.APP _ => bug "unexpected APP"
end

in
    C.collect prog;			(* Collect is way overkill here *)
    (progkind, progname, progargs, lexp M.empty NONE progbody)
end

end
end

