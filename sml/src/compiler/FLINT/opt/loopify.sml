(* copyright 1998 YALE FLINT PROJECT *)
(* monnier@cs.yale.edu *)

signature LOOPIFY =
sig
    val loopify : FLINT.prog -> FLINT.prog
end


structure Loopify :> LOOPIFY =
struct
local
    structure F  = FLINT
    structure O  = Option
    structure M  = IntmapF
    structure S  = IntSetF
    structure LK = LtyKernel
in

fun bug msg = ErrorMsg.impossible ("Loopify: "^msg)
val cplv = LambdaVar.dupLvar

datatype info = I of {tails : int ref, calls: int ref, icalls: int ref, tcp: bool ref, parent: F.lvar}
exception NotFound

fun loopify (prog as (progkind,progname,progargs,progbody)) = let

    val m : info Intmap.intmap = Intmap.new(128, NotFound)

    (* tails: number of tail-recursive calls
     * calls: number of other calls
     * icalls: non-tail self-recursive subset of `calls'
     * tcp: always called in tail-position
     * parent: enclosing function *)
    fun new (f,known,parent) =
	let val i = I{tails=ref 0, calls=ref 0, icalls=ref 0,
		      tcp=ref known, parent=parent}
	in Intmap.add m (f,i); i end

    fun get f = Intmap.map m f

(* collect tries to determine what calls are tail recursive.
 * If a function f is always called in tail position in a function g,
 * then all tail calls to g from f are indeed tail recursive. *)
(* tfs:  we are currently in tail position relative to those functions
 * p:  englobing function *)
fun collect p tfs le = let
    val loop = collect p tfs
in case le
    of F.RET _ => ()
     | F.LET(_,body,le) => (collect p S.empty body; loop le)
     | F.FIX([({isrec=(NONE | SOME(_,F.LK_TAIL)),known,...},f,_,body)],le) =>
       let val I{tcp,calls,icalls,...} = new(f, known, p)
	   val _ = loop le
	   val ecalls = !calls
       in  collect f (if !tcp then S.add(f,tfs) else S.singleton f) body;
	   icalls := !calls - ecalls
       end
     | F.FIX(fdecs,le) =>
       let (* create the new entries in the map *)
	   val fs = map (fn (fk as {known,...},f,_,body) =>
			 (fk, f, body, new(f, false, p)))
			fdecs
	   fun cfun ({isrec,...}:F.fkind,f,body,I{calls,icalls,...}) =
	       let val ecalls = !calls
	       in  collect f (S.singleton f) body;
		   icalls := !calls - ecalls
	       end
       in  loop le;
	   app cfun fs
       end
     | F.APP(F.VAR f,vs) =>
       (let val I{tails,calls,tcp,parent,...} = get f
       in if S.member tfs f then tails := !tails + 1
	  else (calls := !calls + 1;
		if S.member tfs parent then () else tcp := false)
       end handle NotFound => ())
     | F.TFN((_,_,body),le) => (collect p S.empty body; loop le)
     | F.TAPP _ => ()
     | F.SWITCH(v,ac,arms,def) =>
       let fun carm (_,body) = loop body
       in app carm arms; case def of SOME le => loop le | _ => ()
       end
     | (F.CON(_,_,_,_,le) | F.RECORD(_,_,_,le) |
	F.SELECT(_,_,_,le) | F.PRIMOP(_,_,_,le)) => loop le
     | F.RAISE _ => ()
     | F.HANDLE(le,v) => collect p S.empty le
     | F.BRANCH(_,_,le1,le2) => (loop le1; loop le2)

     | F.APP _ => bug "weird F.APP in collect"
end

(* m: int intmap	renaming for function calls
 * tf:(int,int) list	the current functions (if any) and their tail version
 * le:			you get the idea *)
fun lexp m tfs le = let
    val loop = lexp m tfs
in case le
    of F.RET _ => le
     | F.LET(lvs,body,le) => F.LET(lvs, lexp m [] body, loop le)
     | F.FIX(fdecs,le) =>
       let fun cfun (fk:F.fkind as {isrec=SOME(ltys,F.LK_UNKNOWN),cconv,...},
		     f,args,body) =
	       let val I{tcp=ref tcp,icalls=ref icalls,tails=ref tails,...} =
		       get f
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
	       in if icalls = 0 andalso tails = 0
		  then (fk, f, args, lexp m (if tcp then tfs else []) body)
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
			  val tfs' = ((f,ft)::(if tcp then tfs else []))

			  (* make the new body *)
			  val (nargs,nbody) = (args, lexp nm tfs' body)
			  (* wrap into a tail loop if necessary *)
			  val (nargs,nbody) =
			      if tails = 0 then (nargs,nbody) else let
				  val args' = map (fn(v,t) => (cplv v, t)) args
			      in (args',
				  F.FIX([({isrec=SOME(ltys, F.LK_TAIL),
					   known=true, inline=F.IH_SAFE,
					   cconv=cconv'}, ft, nargs,
					  nbody)],
				    F.APP(F.VAR ft, map (F.VAR o #1) args')))
			      end
			  (* wrap into a non-tail loop if necessary *)
			  val (nargs,nbody) =
			      if icalls = 0 then (nargs,nbody) else let
				  val args' = map (fn(v,t) => (cplv v, t)) args
			      in (args',
				  F.FIX([({isrec=SOME(ltys, F.LK_LOOP),
					   known=true, inline=F.IH_SAFE,
					   cconv=cconv'}, fl, nargs,
					  nbody)],
				    F.APP(F.VAR fl, map (F.VAR o #1) args')))
			      end
		      in (fk, f, nargs, nbody)
		      end
	       end
	     | cfun (fk as {inline=F.IH_UNROLL,isrec=SOME _,...},f,args,body) =
	       let val I{tcp=ref tcp,...} = get f
	       in (fk, f, args, lexp m (if tcp then tfs else []) body)
	       end
	     | cfun (fk,f,args,body) =
	       let val I{tcp=ref tcp,...} = get f
	       in (fk, f, args, lexp m (if tcp then tfs else []) body)
	       end
       in F.FIX(map cfun fdecs, loop le)
       end
     | F.APP(F.VAR f,vs) =>
       (let val fl = M.lookup m f
       in case List.find (fn (ft,ft') => ft = f) tfs
	   of SOME(ft, ft') => F.APP(F.VAR ft', vs)
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
     | F.HANDLE(le,v) => F.HANDLE(lexp m [] le, v)
     | F.BRANCH(po,vs,le1,le2) => F.BRANCH(po, vs, loop le1, loop le2)
     | F.PRIMOP(po,vs,lv,le) => F.PRIMOP(po, vs, lv, loop le)

     | F.APP _ => bug "unexpected APP"
end

in
    collect progname S.empty progbody;
    (progkind, progname, progargs, lexp M.empty [] progbody)
end

end
end

