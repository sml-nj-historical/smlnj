(* Copyright 1996 by Bell Laboratories *)
(* cpstrans.sml *)

signature CPSTRANS = sig 
  val cpstrans : CPS.function -> CPS.function
end (* signature CPSTRANS *)

functor CPStrans(MachSpec : MACH_SPEC) : CPSTRANS = struct

local open CPS 
      structure LV = LambdaVar
in
 
fun cpstrans fe = let 

val unboxedfloat = MachSpec.unboxedFloats
val untaggedint = MachSpec.untaggedInt

exception CPSSUBST
val M : value Intmap.intmap = Intmap.new(32,CPSSUBST)
val addvl = Intmap.add M 
fun mapvl v = ((Intmap.map M v) handle CPSSUBST => VAR v)

exception CTYMAP
val CT : cty Intmap.intmap = Intmap.new(32,CTYMAP)
val addty = Intmap.add CT
val getty = Intmap.map CT
fun grabty(VAR v) = ((getty v) handle _ => BOGt)
  | grabty(REAL _) = FLTt
  | grabty(INT _) = INTt
  | grabty(INT32 _) = INT32t
  | grabty _ = BOGt


fun select(i,u,x,ct,ce) = SELECT(i,u,x,ct,ce)
fun record(k,ul,w,ce) = RECORD(k,ul,w,ce)

(* wrappers around floats and ints are now dealt with in the convert phase *)
(***>>
fun unwrapfloat(u,x,ce) = PURE(P.funwrap,[u],x,FLTt,ce)
fun wrapfloat(u,x,ce) = PURE(P.fwrap,[u],x,BOGt,ce)
fun unwrapint(u,x,ce) = PURE(P.iunwrap,[u],x,INTt,ce)
fun wrapint(u,x,ce) = PURE(P.iwrap,[u],x,BOGt,ce)
fun unwrapint32(u,x,ce) = PURE(P.i32unwrap,[u],x,INT32t,ce)
fun wrapint32(u,x,ce) = PURE(P.i32wrap,[u],x,BOGt,ce)

fun select(i,u,x,ct,ce) =
  case (ct,unboxedfloat,untaggedint)
   of (FLTt,true,_) => let val v = LV.mkLvar()
                        in SELECT(i,u,v,BOGt,unwrapfloat(VAR v,x,ce))
                       end
    | (INTt,_,true) => let val v = LV.mkLvar()
                        in SELECT(i,u,v,BOGt,unwrapint(VAR v,x,ce))
                       end
    | (INT32t,_,_)  => let val v = LV.mkLvar()
                        in SELECT(i,u,v,BOGt,unwrapint32(VAR v,x,ce))
                       end
    | _ => SELECT(i,u,x,ct,ce)

fun record(k,ul,w,ce) =
  let fun h((FLTt,u),(l,h)) = 
             if unboxedfloat then 
              (let val v = LV.mkLvar()
                in ((VAR v,OFFp 0)::l, fn ce => wrapfloat(#1 u,v,h(ce)))
               end)
             else (u::l,h)
        | h((INTt,u),(l,h)) = 
             if untaggedint then 
              (let val v = LV.mkLvar()
                in ((VAR v,OFFp 0)::l, fn ce => wrapint(#1 u,v,h(ce)))
               end)
             else (u::l,h)
        | h((INT32t,u),(l,h)) = 
             let val v = LV.mkLvar()
	     in ((VAR v,OFFp 0)::l, fn ce => wrapint32(#1 u,v,h(ce)))
	     end
        | h((_,u),(l,h)) = (u::l,h)

      val info = map (fn (u as (v,_)) => (grabty v,u)) ul
      val (nul,header) = fold h info ([],fn x => x)
   in header(RECORD(k,nul,w,ce))
  end
<<***)

fun cexptrans(ce) = 
  case ce 
   of RECORD(k,vl,w,ce) => record(k,map rectrans vl,w,cexptrans ce)
    | SELECT(i,v,w,t,ce) => 
          (let val _ = addty(w,t)
               val v' = vtrans v
               val ce' = cexptrans ce
            in select(i, v', w, getty w, ce')
           end)
    | OFFSET(i,v,w,ce) => OFFSET(i, vtrans v, w, cexptrans ce)
    | APP(v,vl) => APP(vtrans v, map vtrans vl)
    | FIX(l,ce) => FIX(map functrans l, cexptrans ce)
    | SWITCH(v,c,l) => SWITCH(vtrans v,c,map cexptrans l)
    | LOOKER(p,vl,w,t,ce) => 
          (let val _ = addty(w,t)
               val vl' = map vtrans vl
               val ce' = cexptrans ce
            in LOOKER(p, vl', w, getty w, ce')
           end)
    | SETTER(p,vl,ce) => 
          SETTER(p, map vtrans vl, cexptrans ce)
    | ARITH(p,vl,w,t,ce) => 
          (addty(w,t); ARITH(p, map vtrans vl, w, t, cexptrans ce))

    
  (*** this special case is a temporary hack; ask ZHONG for details *) 
(*
    | PURE(P.wrap,[u],w,t as PTRt(FPT _),ce) => 
          (addty(w, t); PURE(P.wrap, [vtrans u], w, t, cexptrans ce))
    | PURE(P.unwrap,[u],w,t as PTRt(FPT _),ce) => 
          (addty(w, t); PURE(P.unwrap, [vtrans u], w, t, cexptrans ce))
*)

    | PURE(P.wrap,[u],w,t,ce) => 
          (addvl(w,vtrans u); cexptrans ce)
    | PURE(P.unwrap,[u],w,t,ce) => 
          (case u of VAR z => addty(z,t)
                   | _ => ();
           addvl(w,vtrans u); cexptrans ce) 
    | PURE(P.fwrap,[u],w,t,ce) => 
          if unboxedfloat 
          then (addty(w,t); PURE(P.fwrap,[vtrans u],w,t,cexptrans ce))
          else (addvl(w,vtrans u); cexptrans ce)
    | PURE(P.funwrap,[u],w,t,ce) => 
          if unboxedfloat 
          then (addty(w,t); PURE(P.funwrap,[vtrans u],w,t,cexptrans ce))
          else (addvl(w,vtrans u); cexptrans ce)
    | PURE(P.iwrap,[u],w,t,ce) => 
          if untaggedint
          then (addty(w,t); PURE(P.iwrap,[vtrans u],w,t,cexptrans ce))
          else (addvl(w,vtrans u); cexptrans ce)
    | PURE(P.iunwrap,[u],w,t,ce) => 
          if untaggedint
          then (addty(w,t); PURE(P.iunwrap,[vtrans u],w,t,cexptrans ce))
          else (addvl(w,vtrans u); cexptrans ce)
    | PURE(P.i32wrap,[u],w,t,ce) =>
	      (addty(w,t); PURE(P.i32wrap,[vtrans u],w,t,cexptrans ce))
    | PURE(P.i32unwrap,[u],w,t,ce) =>
	      (addty(w,t); PURE(P.i32unwrap,[vtrans u],w,t,cexptrans ce))
(*
    | PURE(P.cast,[u],w,_,ce) =>
          (addvl(w,vtrans u); cexptrans ce)
*)
    | PURE(P.getcon,[u],w,t,ce) =>
          (addty(w,t); select(0,vtrans u,w,t,cexptrans ce))
    | PURE(P.getexn,[u],w,t,ce) =>
          (addty(w,t); select(0,vtrans u,w,t,cexptrans ce))
    | PURE(p,vl,w,t,ce) => 
          (let val _ = addty(w,t)
               val vl' = map vtrans vl
               val ce' = cexptrans ce
            in PURE(p, vl', w, getty w, ce')
           end)
    | BRANCH(p,vl,c,e1,e2) => 
          BRANCH(p, map vtrans vl, c, cexptrans e1, cexptrans e2)

and functrans(fk,v,args,cl,ce) = 
      let val _ = ListPair.app addty (args,cl)
          val ce' = cexptrans ce
       in (fk,v,args,map getty args,ce')
      end
and rectrans(v,acp) = (vtrans v,acp)
and vtrans(VAR v) = (mapvl v) | vtrans u = u

 in functrans fe
end


end (* toplevel local *)
end (* structure CPStrans *)


(*
 * $Log: cpstrans.sml,v $
 * Revision 1.2  1998/01/07 15:10:59  dbm
 *   Fixing bug 1323. Wrapping and unwrapping primitives were usually ignored
 *   in the cpstrans phase before we perform the cps optimization. Unfortunately,
 *   they could lead to ill-typed CPS programs. To resolve this, I turn those
 *   sensitive wrap and unwrap primitives into "casts"; I leave the casts in the
 *   code; the cps generic phase will generate a move for each cast. In the
 *   long term, we have to think thoroughly about the meanings of these wrapping
 *   primitives and how they interface with compile-time optimizations.
 *
 * Revision 1.1.1.1  1997/01/14 01:38:30  george
 *   Version 109.24
 *
 *)
