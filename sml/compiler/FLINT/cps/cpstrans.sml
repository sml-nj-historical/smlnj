(* cpstrans.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature CPSTRANS =
  sig
    val cpstrans : CPS.function -> CPS.function
  end (* signature CPSTRANS *)

functor CPStrans(MachSpec : MACH_SPEC) : CPSTRANS = struct

local open CPS
      structure LV = LambdaVar
in

fun bug s = ErrorMsg.impossible ("CPStrans: " ^ s)
fun ident x = x
val mkv = LV.mkLvar

(**************************************************************************
 *                    TOP OF THE MAIN FUNCTION                            *
 **************************************************************************)
fun cpstrans fe = let

val unboxedfloat = MachSpec.unboxedFloats
val untaggedint = MachSpec.untaggedInt

exception CPSSUBST
val M : value IntHashTable.hash_table = IntHashTable.mkTable(32,CPSSUBST)
val addvl = IntHashTable.insert M
fun mapvl v = ((IntHashTable.lookup M v) handle CPSSUBST => VAR v)

exception CTYMAP
val CT : cty IntHashTable.hash_table = IntHashTable.mkTable(32,CTYMAP)
val addty = IntHashTable.insert CT
val getty = IntHashTable.lookup CT
fun grabty (VAR v) = ((getty v) handle _ => BOGt)
  | grabty (REAL _) = FLTt 64	(* REAL32: FIXME need size info for real literals *)
  | grabty (INT _) = TINTt
  | grabty (INT32 _) = INTt 32  (* 64BIT: will need 64-bit integer literals *)
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
   of (FLTt,true,_) => let val v = mkv()
                        in SELECT(i,u,v,BOGt,unwrapfloat(VAR v,x,ce))
                       end
    | (INTt,_,true) => let val v = mkv()
                        in SELECT(i,u,v,BOGt,unwrapint(VAR v,x,ce))
                       end
    | (INT32t,_,_)  => let val v = mkv()
                        in SELECT(i,u,v,BOGt,unwrapint32(VAR v,x,ce))
                       end
    | _ => SELECT(i,u,x,ct,ce)

fun record(k,ul,w,ce) =
  let fun h((FLTt,u),(l,h)) =
             if unboxedfloat then
              (let val v = mkv()
                in ((VAR v,OFFp 0)::l, fn ce => wrapfloat(#1 u,v,h(ce)))
               end)
             else (u::l,h)
        | h((INTt,u),(l,h)) =
             if untaggedint then
              (let val v = mkv()
                in ((VAR v,OFFp 0)::l, fn ce => wrapint(#1 u,v,h(ce)))
               end)
             else (u::l,h)
        | h((INT32t,u),(l,h)) =
             let val v = mkv()
	     in ((VAR v,OFFp 0)::l, fn ce => wrapint32(#1 u,v,h(ce)))
	     end
        | h((_,u),(l,h)) = (u::l,h)

      val info = map (fn (u as (v,_)) => (grabty v,u)) ul
      val (nul,header) = fold h info ([], ident)
   in header(RECORD(k,nul,w,ce))
  end
<<***)


(**************************************************************************
 *          UTILITY FUNCTIONS THAT DO THE ARGUMENT SPILLING               *
 **************************************************************************)

(** the following figures must be consistent with the choices made
    in the closure or spilling phases *)
val fpnum = Int.min(MachSpec.numFloatRegs-2, MachSpec.numArgRegs)
val nregs = MachSpec.numRegs - MachSpec.numCalleeSaves
val gpnum = Int.min(nregs - 3, MachSpec.numArgRegs)

fun argSpill (args, ctys) =
  let fun h([], [], ngp, nfp, ovs, ots, [], [], []) = NONE
        | h([], [], ngp, nfp, ovs, ots, [x], [_], []) = NONE
        | h([], [], ngp, nfp, ovs, ots, gvs, gts, fvs) =
           SOME(rev ovs, rev ots, rev gvs, rev gts, rev fvs)
        | h(x::xs, ct::cts, ngp, nfp, ovs, ots, gvs, gts, fvs) =
           (case ct
             of FLTt 64 =>
                 if nfp > 0 then
                   h(xs, cts, ngp, nfp-1, x::ovs, ct::ots, gvs, gts, fvs)
                 else
                   h(xs, cts, ngp, nfp, ovs, ots, gvs, gts, x::fvs)
	      | FLTt _ => raise Fail "unsupported FLTt size" (* REAL32: FIXME *)
              | _ =>
                 if ngp > 0 then
                   h(xs, cts, ngp-1, nfp, x::ovs, ct::ots, gvs, gts, fvs)
                 else
                   h(xs, cts, ngp, nfp, ovs, ots, x::gvs, ct::gts, fvs))
        | h _ = bug "unexpected case in argSpill"

      val n = length args
   in if (n > fpnum) orelse (n > gpnum) then
        h (args, ctys, gpnum, fpnum, [], [], [], [], [])
      else NONE
  end (* function argSpill *)

fun spillIn (origargs, origctys, spgvars, spgctys, spfvars) =
  let val (fhdr, spgvars, spgctys) =
        case spfvars
         of [] => (ident, spgvars, spgctys)
          | _ => let val v = mkv()
                     val vs = map (fn x => (x, OFFp 0)) spfvars
                     val ct = PTRt(FPT (length vs))
                     val fh = fn e => RECORD(RK_FBLOCK, vs, v, e)
                  in (fh, (VAR v)::spgvars, ct::spgctys)
                 end
      val (spgv, ghdr) =
        case spgvars
         of [] => (NONE, fhdr)
          | [x] => (SOME x, fhdr)
          | _ => let val v = mkv()
                     val vs = map (fn x => (x, OFFp 0)) spgvars
                  in (SOME (VAR v), fn e => fhdr(RECORD(RK_RECORD, vs, v, e)))
                 end
   in case spgv of SOME x => SOME(origargs@[x], ghdr)
                 | NONE => NONE
  end

fun spillOut (origargs, origctys, spgvars, spgctys, spfvars) =
  let val (spfv, fhdr, spgvars, spgctys) =
        case spfvars
         of [] => (NONE, ident, spgvars, spgctys)
          | _ => let val v = mkv()
                     val u = VAR v
                     fun g (sv, (i,hdr)) =
                       (i+1, fn e => hdr(SELECT(i, u, sv, FLTt 64, e)))  (* REAL32: FIXME *)
                     val (n,fh) = foldl g (0, ident) spfvars
                     val ct = PTRt(FPT n)
                  in (SOME v, fh, v::spgvars, ct::spgctys)
                 end
      val (spgv, ghdr) =
        case (spgvars, spgctys)
         of ([], _) => (NONE, fhdr)
          | ([x], t::_) => (SOME (x,t), fhdr)
          | _ => let val v = mkv()
                     val u = VAR v
                     fun g (sv, st, (i,hdr)) =
                       (i+1, fn e =>hdr(SELECT(i, u, sv, st, e)))
                     val (n, gh) = ListPair.foldl g (0, fhdr) (spgvars,spgctys)
                     val ct = PTRt(RPT n)
                  in (SOME (v, ct), gh)
                 end
   in case spgv of SOME (x,t) => SOME (origargs@[x], origctys@[t], ghdr)
                 | NONE => NONE
  end

(* mkargin : value list -> (cexp -> cexp * value list) option *)
fun mkargin (args : value list) =
  let val ctys = map grabty args
   in case argSpill (args, ctys)
       of SOME xx => spillIn xx
        | NONE => NONE
  end

(* mkargout : lvar list -> (lvar list * cty list * cexp -> cexp) option *)
fun mkargout args =
  let val ctys = map getty args
   in case argSpill (args, ctys)
       of SOME xx => spillOut xx
        | NONE => NONE
  end

(**************************************************************************
 *              MAIN FUNCTIONS THAT TRANSLATE CPS CODE                    *
 **************************************************************************)
fun cexptrans(ce) =
  case ce
   of RECORD(k,vl,w,ce) => record(k,map rectrans vl,w,cexptrans ce)
    | SELECT(i,v,w,t,ce) =>
          let val _ = addty(w,t)
              val v' = vtrans v
              val ce' = cexptrans ce
           in select(i, v', w, getty w, ce')
          end
    | OFFSET(i,v,w,ce) => OFFSET(i, vtrans v, w, cexptrans ce)
    | APP(v,vl) =>
          (case mkargin vl
            of SOME (nvl, hdr) => cexptrans(hdr(APP(v, nvl)))
             | NONE =>  APP(vtrans v, map vtrans vl))
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
    | RCC(k,l,p,vl,wtl,ce) =>
          (app addty wtl;
	   RCC(k, l, p, map vtrans vl, wtl, cexptrans ce))


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
       in (case mkargout args
            of SOME (nargs, nctys, fhdr) =>
                (fk, v, nargs, nctys, fhdr ce')
             | NONE => (fk, v, args, cl, ce'))
      end

and rectrans(v,acp) = (vtrans v,acp)

and vtrans(VAR v) = (mapvl v) | vtrans u = u

 in functrans fe
end


end (* toplevel local *)
end (* structure CPStrans *)

