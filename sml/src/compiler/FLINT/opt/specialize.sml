(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* specialize.sml *)

(* minimal type derivation, type specialization,  and lifting of
   structure access (not supported yet) and type application *)

signature SPECIALIZE =
sig 
  val specLexp : Lambda.lexp -> Lambda.lexp

end (* signature SPECIALIZE *)

structure Specialize : SPECIALIZE = 
struct 

local structure LD = LtyDef
      structure LT = LtyExtern
      structure DI = DebIndex
      structure PT = PrimTyc
      open Lambda
in

val say = Control.Print.say
fun bug s = ErrorMsg.impossible ("Specialize: " ^ s)
val mkv = LambdaVar.mkLvar
val ident = fn le : Lambda.lexp => le
fun tvar i = LT.tcc_var(DI.innermost, i)

fun mktvs ks = 
  let fun h (_::r, i, z) = h(r, i+1, (tvar i)::z)
        | h ([], _, z) = rev z
   in h (ks, 0, [])
  end

(* the special box tkind *)
val tk_tbx = LT.tkc_box
val tk_tmn = LT.tkc_mono
val tk_eqv = LT.tk_eqv

(* checking the equivalence of two tyc sequences *)
val tc_eqv = LT.tc_eqv
fun tcs_eqv (xs, ys) = 
  let fun teq(a::r, b::s) = if tc_eqv(a, b) then teq(r, s) else false
        | teq([],[]) = true
        | teq _ = bug "unexpected cases in tcs_eqv"
   in teq(xs, ys)
  end

(****************************************************************************
 *                  UTILITY FUNCTIONS FOR KIND AND TYPE BOUNDS              *
 ****************************************************************************)

(* 
 * Bnd is a lattice on the type hierarchy, used to infer minimum type bounds;
 * Right now, we only deal with first-order kinds. All higher-order kinds
 * will be assigned KTOP.
 *)
datatype bnd 
  = KBOX
  | KTOP
  | TBND of LD.tyc

type bnds = bnd list 

datatype dinfo 
  = ESCAPE
  | NOCSTR
  | CSTR of bnds

(** THE FOLLOWING FUNCTION IS NOT FULLY DEFINED *)
fun kBnd kenv tc = 
  (if LT.tcp_var tc then
     (let val (i,j) = LT.tcd_var tc
          val (_,ks) = List.nth(kenv, i-1) 
                            handle _ => bug "unexpected case A in kBnd"
             val k = List.nth(ks, j)
                            handle _ => bug "unexpected case B in kBnd"
       in if tk_eqv(tk_tbx, k) then KBOX else KTOP
      end)
   else if LT.tcp_prim tc then
          (let val p = LT.tcd_prim tc
            in if PT.unboxed p then KTOP else KBOX
           end)
        else KBOX)

fun kmBnd kenv (tc, KTOP) = KTOP
  | kmBnd kenv (tc, KBOX) = kBnd kenv tc
  | kmBnd kenv (tc, TBND _) = bug "unexpected cases in kmBnd"

fun tBnd kenv tc = TBND tc

fun tmBnd kenv (tc, KTOP) = KTOP
  | tmBnd kenv (tc, KBOX) = kBnd kenv tc
  | tmBnd kenv (tc, x as TBND t) = 
      if tc_eqv(tc, t) then x else kmBnd kenv (tc, kBnd kenv t)

(* 
 * Given a list of bnd information, return a list of filter info;
 * if all bounds are of TBND form, we got a full specialization, 
 * we return NONE. 
 *)
fun bndFlt bnds = 
  let fun h ((TBND _)::bs, r, z) = h(bs, false::r, z)
        | h (_::bs, r, _) = h(bs, true::r, false)
        | h ([], r, z) = if z then NONE else SOME (rev r)
   in h(bnds, [], true)
  end

(*
 * Given a list of default kinds, and a list of bnd information, and a 
 * flag indicating whether it is full specialization;  
 * two pieces of information: resOp of (tkind list option * tyc list) option
 * and the filterOp of (bool list) option
 *)
fun bndGen(oks, bnds, fltOp, d) = 
  let val adj = case fltOp of NONE => (fn tc => tc)
                            | _ => (fn tc => LT.tc_adj(tc, d, DI.next d))
        (* no full-specializations, so we push one-level down *)

      fun h([], [], i, [], ts, b) = (NONE, rev ts, b)
        | h([], [], i, ks, ts, b) = (SOME(rev ks), rev ts, b)
        | h(ok::oks, (TBND tc)::bs, i, ks, ts, b) = 
             h(oks, bs, i, ks, (adj tc)::ts, false)
        | h(ok::oks, KTOP::bs, i, ks, ts, b) = 
             h(oks, bs, i+1, ok::ks, (tvar i)::ts, b)
        | h(ok::oks, KBOX::bs, i, ks, ts, b) = 
             let (* val nk = if tk_eqv(tk_tbx, ok) then ok else tk_tbx *)
                 val nk = if tk_eqv(tk_tmn, ok) then tk_tbx else ok
              in h(oks, bs, i+1, nk::ks, (tvar i)::ts, b)
             end
        | h _ = bug "unexpected cases in bndGen"

      val (ksOp, ts, boring) = h(oks, bnds, 0, [], [], true)
   in if boring then ((ksOp, NONE), NONE)    
      else ((ksOp, SOME ts), SOME fltOp) 
  end


(****************************************************************************
 *                  UTILITY FUNCTIONS FOR INFO ENVIRONMENTS                 *
 ****************************************************************************)

(*
 * We maintain a table mapping each lvar to a list of its use, 
 * indexed by its specific type instances. 
 *)
exception ITABLE
exception DTABLE

type depth = DI.depth
type tkind = LD.tkind
type info = (tyc list * lvar) list
type itable = info Intmap.intmap
type dtable = (depth * dinfo) Intmap.intmap
datatype infoEnv = IENV of (itable * tkind list) list * dtable 

(** initializing a new info environment : unit -> infoEnv *)
fun initInfoEnv () = 
  let val itable : itable = Intmap.new (32, ITABLE)  
      val dtable : dtable = Intmap.new(32, DTABLE)
   in IENV ([(itable,[])], dtable)
  end

(** register a definition of sth interesting into the info environment *)
fun entDtable (IENV(_, dtable), v, ddinfo) = Intmap.add dtable (v, ddinfo)

(** mark an lvar in the dtable as escape *)
fun escDtable (IENV(_, dtable), v) = 
  ((case Intmap.map dtable v
     of (_, ESCAPE) => ()
      | (d, _) => Intmap.add dtable (v, (d, ESCAPE)))
   handle _ => ())

(*
 * Register a dtable entry; modify the least upper bound of a particular
 * type binding; notice I am only moving kind info upwards, not type
 * info, I could move type info upwards though, but it is just some
 * extra complications. 
 *)
fun regDtable (IENV(kenv, dtable), v, infos) = 
  let val (dd, dinfo) = 
        ((Intmap.map dtable v) handle _ => 
                bug "unexpected cases in regDtable")
   in (case dinfo 
        of ESCAPE => ()
         | _ => 
             (let fun h ((ts, _), ESCAPE) = ESCAPE
                    | h ((ts, _), NOCSTR) = CSTR (map (kBnd kenv) ts)
                    | h ((ts, _), CSTR bnds) = 
                        let val nbnds = ListPair.map (kmBnd kenv) (ts, bnds)
                         in CSTR nbnds
                        end
                  val ndinfo = foldr h dinfo infos
               in Intmap.add dtable (v, (dd, ndinfo))
              end))
  end

(* 
 * Calculate the least upper bound of all type instances;
 * this should take v out of the current dtable ! 
 *)
fun sumDtable(IENV(kenv, dtable), v, infos) = 
  let val (dd, dinfo) = 
        ((Intmap.map dtable v) handle _ => 
                bug "unexpected cases in sumDtable")
   in (case dinfo
        of ESCAPE => (dd, ESCAPE)
         | _ => 
             (let fun h ((ts, _), ESCAPE) = ESCAPE
                    | h ((ts, _), NOCSTR) = CSTR (map (tBnd kenv) ts)
                    | h ((ts, _), CSTR bnds) = 
                        let val nbnds = ListPair.map (tmBnd kenv) (ts, bnds)
                         in CSTR nbnds
                        end
                  val ndinfo = foldr h dinfo infos
               in (dd, ndinfo)
              end))
  end

(** look and add a new type instance into the itable *)
fun lookItable (IENV (itabs,dtab), d, v, ts) = 
  let val (dd, _) = 
        ((Intmap.map dtab v) handle _ => bug "unexpected cases in lookItable")

      val nd = Int.max(dd, LT.tcs_depth(ts, d))

      val (itab,_) = ((List.nth(itabs, d-nd)) handle _ => 
                      bug "unexpected itables in lookItable")
    
      val nts = map (fn t => LT.tc_adj(t, d, nd)) ts
      val xi = (Intmap.map itab v) handle _ => []

      fun h ((ots,x)::r) = if tcs_eqv(ots, nts) then (VAR x) else h r
        | h [] = let val nv =  mkv() 
                     val _ = Intmap.add itab (v, (nts, nv)::xi)
                  in VAR nv
                 end
   in h xi
  end

(** push a new layer of type abstraction : infoEnv -> infoEnv *)
fun pushItable (IENV(itables, dtable), ks) = 
  let val nt : itable = Intmap.new(32, ITABLE)
   in (IENV((nt,ks)::itables, dtable))
  end

(*
 * Pop off a layer when exiting a type abstaction, adjust the dtable properly,
 * and generate the proper headers: infoEnv -> (lexp -> lexp)
 *)
fun popItable (IENV([], _)) =
      bug "unexpected empty information env in popItable"
  | popItable (ienv as IENV((nt,_)::_, _)) = 
      let val infos = Intmap.intMapToList nt
          fun h ((v,info), hdr) = 
            let val _ = regDtable(ienv, v, info)
                fun g ((ts, x), e) = LET(x, TAPP(VAR v, ts), e)
             in fn e => foldr g (hdr e) info
            end
       in foldr h ident infos 
      end

(* Check out a escaped variable from the info env, build the header properly *)
fun chkOutEsc (IENV([], _), v) =
      bug "unexpected empty information env in chkOut"
  | chkOutEsc (ienv as IENV((nt,_)::_, _), v) = 
      let val info = (Intmap.map nt v) handle _ => []
          fun g ((ts, x), e) = LET(x, TAPP(VAR v, ts), e)
          val hdr = fn e => foldr g e info
          val _ = Intmap.rmv nt v  (* so that v won't be considered again *)
       in hdr
      end

(* 
 * Check out a regular variable from the info env, build the header
 * properly, of course, adjust the corresponding dtable entry.
 *)
fun chkOutNorm (IENV([], _), v, oks, d) =
      bug "unexpected empty information env in chkOut"

  | chkOutNorm (ienv as IENV((nt,_)::_, dtable), v, oks, d) = 
      let val info = (Intmap.map nt v) handle _ => []
          val (dd, dinfo) = sumDtable(ienv, v, info)
          val (resOp, filterOp) =
            (case dinfo
              of ESCAPE => ((NONE,NONE), NONE)
               | NOCSTR => (* must be a dead function, let's double check *) 
                   (case info of [] => ((NONE,NONE), NONE)
                               | _ => bug "unexpected cases in chkOutNorm")
               | CSTR bnds => bndGen(oks, bnds, bndFlt bnds, d))

          fun tapp(e, ts, NONE) = TAPP(e, ts)
            | tapp(e, ts, SOME (NONE)) = SVAL e
            | tapp(e, ts, SOME (SOME flags)) = 
                let fun h([], [], z) = rev z
                      | h(a::r, b::s, z) =
                          if b then h(r, s, a::z) else h(r, s, z)
                      | h _ = bug "unexpected cases in tapp"
                 in TAPP(e, h(ts, flags, []))
                end

          fun g ((ts, x), e) = LET(x, tapp(VAR v, ts, filterOp), e)
          val hdr = fn e => foldr g e info
          val _ = Intmap.rmv nt v  (* so that v won't be considered again *)
       in (hdr, resOp)
      end

(****************************************************************************
 *                         MAIN FUNCTIONS                                   *
 ****************************************************************************)

(*
 * Function transform has the following type:
 *
 *    infoEnv * lty cvt * tyc cvt * DI.depth -> (lexp -> lexp) 
 *
 *  where type 'a cvt = DI.depth -> 'a -> 'a
 *
 * The 2nd and 3rd arguments are used to encode the necessary type 
 * translations. The 4th argument is the depth the resulting expression
 * will be at.
 *)
fun transform (ienv, ltf, tcf, d) = 
let 

fun lpsv sv = 
  (case sv
    of (INT _ | WORD _ | INT32 _ | WORD32 _ | REAL _ | STRING _) => sv
     | VAR v => (escDtable(ienv, v); sv)
     | PRIM (p, lt, ts) => PRIM(p, ltf d lt, map (tcf d) ts)
         (* I don't think this is really necessary because all primops
            have closed types, but probably it is quite cheap *)
     | GENOP(dict, p, lt, ts) => GENOP(dict, p, ltf d lt, map (tcf d) ts))

fun loop le = 
  (case le
    of SVAL sv => SVAL(lpsv sv)
     | TAPP(VAR v, ts) => 
         (SVAL(lookItable(ienv, d, v, map (tcf d) ts)))
     | TAPP(sv, ts) => TAPP(lpsv sv, map (tcf d) ts)

     | TFN(ks, e) => 
         let val nienv = pushItable(ienv, ks)
             val nd = DI.next d
             val ne = transform (nienv, ltf, tcf, nd) e
             val hdr = popItable nienv
          in TFN(ks, hdr ne)
         end

     | LET(v, e1 as TFN(ks, be1), e2) => 
         let val _ = entDtable(ienv, v, (d,NOCSTR))
             val ne2 = loop e2 
             val (hdr, resOp) = chkOutNorm(ienv, v, ks, d)  
             val ne1 = 
               (case resOp
                 of (NONE, NONE) => loop e1
                  | (SOME nks, NONE) => loop(TFN(nks, be1))
                  | (NONE, SOME nts) => 
                     let fun nltf nd lt = 
                           (LT.lt_sp_adj(ks, ltf (DI.next nd) lt, nts, nd-d, 0))
                         fun ntcf nd tc = 
                           (LT.tc_sp_adj(ks, tcf (DI.next nd) tc, nts, nd-d, 0))
                      in transform (ienv, nltf, ntcf, d) be1
                     end
                     (** this unfortunately relies on the value restrictions *)

                  | (SOME nks, SOME nts) => 
                     (** assume nts is already shifted one level down *)
                     let val nienv = pushItable(ienv, nks)
                         val xd = DI.next d                         

                         fun nltf nd lt = 
                           let val lt1 = LT.lt_sp_sink(ks, lt, d, nd)
                               val lt2 = ltf (DI.next nd) lt1
                            in (LT.lt_sp_adj(ks, lt2, nts, nd-xd, 0))
                           end
                         fun ntcf nd tc = 
                           let val tc1 = LT.tc_sp_sink(ks, tc, d, nd)
                               val tc2 = tcf (DI.next nd) tc1
                            in (LT.tc_sp_adj(ks, tc2, nts, nd-xd, 0))
                           end
                         val nbe1 = transform (nienv, nltf, ntcf, xd) be1
                         val hdr0 = popItable nienv
                      in (TFN(nks, hdr0 nbe1))
                     end)
          in LET(v, ne1, hdr ne2)
         end

     | LET(v, e1, e2) => 
         let val _ = entDtable(ienv, v, (d,ESCAPE))
             val ne2 = loop e2
             val hdr = chkOutEsc(ienv, v)
          in LET(v, loop e1, hdr ne2)
         end

     | FN(v, t, e) => 
         let val _ = entDtable(ienv, v, (d,ESCAPE))
             val ne = loop e
             val hdr = chkOutEsc(ienv, v)
          in FN(v, ltf d t, hdr ne)
         end

     | FIX(vs, ts, es, eb) => FIX(vs, map (ltf d) ts, map loop es, loop eb)
         (* ASSUMPTIONS WE MADE HERE: all lvars defined in vs can't be 
            polymorphic functions, that is, all ltys in ts must be 
            monomorphic types *)

     | APP(sv1, sv2) => APP(lpsv sv1, lpsv sv2)

     | PACK (lt, ts, nts, sv) => 
         PACK(ltf d lt, map (tcf d) ts, map (tcf d) nts, lpsv sv)

     | CON ((s,r,lt), ts, sv) => 
         CON((s, r, ltf d lt), map (tcf d) ts, lpsv sv)

     | DECON ((s,r,lt), ts, sv) => 
         DECON((s, r, ltf d lt), map (tcf d) ts, lpsv sv)

     | SWITCH (sv, reps, cases, opp) => 
         let val nsv = lpsv sv
             val ncases = map (fn (c, x) => (c, loop x)) cases
             val nopp = (case opp of NONE => NONE
                                   | SOME x => SOME(loop x))
          in SWITCH(nsv, reps, ncases, nopp)
         end

     | RECORD vs => RECORD (map lpsv vs)
     | SRECORD vs => SRECORD (map lpsv vs)
     | VECTOR (vs, t) => VECTOR(map lpsv vs, tcf d t)
     | SELECT (i, sv) => SELECT(i, lpsv sv)
     | ETAG (sv, t) => ETAG(lpsv sv, ltf d t)
     | RAISE (sv, t) => RAISE(lpsv sv, ltf d t)
     | HANDLE (e, sv) => HANDLE(loop e, lpsv sv)
     | _ => bug "unexpected lambda expression in transform")

 in loop 
end (* function transform *)

(* Definition of the main function *)
fun specLexp (FN(v, t, e)) = 
      let val tcf = fn (d : DI.depth) => fn (x : LD.tyc) => x
          val ltf = fn (d : DI.depth) => fn (x : LD.lty) => x
          val ienv = initInfoEnv()
          val d = DI.top
          val _ = entDtable(ienv, v, (d, ESCAPE))
          val ne = transform (ienv, ltf, tcf, d) e
          val hdr = chkOutEsc(ienv, v)

          (*** invariant: itable should be empty ! ***)
       in FN(v, t, hdr ne)
      end
  | specLexp _ = bug "unexpected lambda expressions specLexp"

end (* toplevel local *)
end (* structure Specialize *)

