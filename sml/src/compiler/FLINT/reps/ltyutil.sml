(* Copyright (c) 1997 YALE FLINT PROJECT *)
(* ltyutil.sml *)

(*** this file will go away soon *)

signature LTYUTIL = sig 

type tkind = LtyDef.tkind
type tyc = LtyDef.tyc
type lty = LtyDef.lty

(** used by the coercion and wrapping *)
val tcWrap : tyc -> tyc option
val genWrap : bool -> ((tyc -> tyc option) * (lty -> lty option)
                       * (tyc list -> tyc list option))

val genWrapNEW : bool -> ((tyc -> tyc) * (lty -> lty) *
                          (tyc -> tyc) * (lty -> lty) * (unit -> unit))
end  (* signature LTYUTIL *)

structure LtyUtil : LTYUTIL = 
struct 

local structure DA = Access
      structure DI = DebIndex
      structure LT = LtyExtern
      structure PO = PrimOp
      structure PT = PrimTyc
      open LtyKernel
in 

fun bug s = ErrorMsg.impossible ("LtyUtil: " ^ s)
val say = Control.Print.say
fun fromto(i,j) = if i < j then (i::fromto(i+1,j)) else []

fun option(NONE) = false
  | option(SOME _) = true

fun exists(p, a::r) = if p a then true else exists(p, r)
  | exists(p, []) = false

fun opList l = exists(option, l)


type tkind = LtyDef.tkind
type tyc = LtyDef.tyc
type lty = LtyDef.lty
type tkindEnv = LT.tkindEnv

structure TcDict = BinaryDict(struct type ord_key = tyc
                                     val cmpKey = tc_cmp
                              end)

structure LtDict = BinaryDict(struct type ord_key = lty
                                     val cmpKey = lt_cmp
                              end)

(** wrapping over a lambdatyc; assumption: arg is in normal form already *)
(** warning: this does not handle tycons of non-zero arity *)
datatype ucvinfo = SOMEB of tyc
                 | SOMEU of tyc
                 | NOTHING

fun uinfoList l = exists(fn NOTHING => false | _ => true, l)

val tcBox = LT.tcc_box 

fun genWrap save = 
let 
val m1 = ref (TcDict.mkDict())
fun lookTc t = 
  if save then 
    let val u = !m1
     in (case TcDict.peek(u, t)
          of SOME t' => t'
           | NONE => 
              let val x = tcWrap t
                  val _ = (m1 := TcDict.insert(u, t, x))
               in x
              end)
    end
  else tcWrap t


and tcWrap x = 
  (case (tc_out x)
    of (TC_PRIM pt) =>  
         if PT.unboxed pt then SOME (tcBox x) else NONE 
         (* if (PT.isvoid pt) then NONE else SOME (tcBox x) *)
         (* warning: this does not handle tycons of non-zero arity *)
     | TC_TUPLE _ => SOME(ucvInfo x) 
     | TC_ARROW _ => SOME(ucvInfo x)
     | (TC_FN(ks, tc)) => 
         (case (tc_out tc, lookTc tc)
           of (TC_SEQ _, NONE) => NONE
            | (TC_PRIM _, NONE) => NONE
            | (TC_FN _, _) => bug "unexpected case in tcWrap"
            | (_, NONE) => SOME(LT.tcc_fn(ks, tcBox tc))
                (** invariants: any TC_FN whose body is not TC_SEQ
                 must have a body of kind Omega; a temporary hack **)
            | (_, SOME z) => SOME(LT.tcc_fn(ks, z)))
     | (TC_APP(tc, ts)) => 
         (case lookTc tc of NONE => NONE
                          | SOME z => SOME(LT.tcc_app(z, ts)))
     | (TC_SEQ ts) => 
         (case tcsWrap ts of NONE => NONE
                           | SOME z => SOME(LT.tcc_seq z))
     | _ => NONE)

and ucvInfo x =
  (case tcUncover x
    of NOTHING => tcBox x 
     | SOMEB y => y
     | SOMEU z => tcBox z)

and tcsWrap xs = 
  let fun p([], flag, bs) = if flag then SOME(rev bs) else NONE
        | p(a::r, flag, bs) = 
            (case (lookTc a) of NONE => p(r, flag, a::bs)
                              | SOME z => p(r, true, z::bs))
   in p(xs, false, [])
  end

and ltWrap x = 
  (case lt_out x
    of LT_TYC t => (case lookTc t
                     of NONE => NONE
                      | SOME z => SOME(LT.ltc_tyc z))
     | _ => bug "unexpected case in ltWrap")

(*** wrapping for partially-boxed representations ***)
and tcUncover x = 
  (case (tc_out x)
    of (TC_PRIM pt) => NOTHING
     | (TC_VAR _ | TC_PROJ _ | TC_ABS _ | TC_NVAR _) => SOMEU x
     | (TC_TUPLE (_,ts)) => 
         let val nts = map tcUncover ts
          in if (uinfoList nts) then 
               (let fun h(z, NOTHING) = z
                      | h(_, SOMEB z) = z
                      | h(_, SOMEU z) = z
                    val nt = LT.tcc_tuple (ListPair.map h (ts, nts))
                 in SOMEB(tcBox nt)
                end)
             else NOTHING
         end
     | (TC_ARROW _) => 
         let val (tc1, tc2) = LT.tcd_parrow x
             val ntc1 = 
               (case tc_out tc1
                 of TC_TUPLE (_, ts as [_, _]) =>
                      let val nts = map lookTc ts
                       in if (opList nts) then 
                           let fun h(z, NONE) = z
                                 | h(_, SOME z) = z
                               val nt = LT.tcc_tuple(ListPair.map h (ts, nts))
                            in SOMEU nt
                           end
                          else NOTHING
                      end
                  | (TC_VAR _ | TC_PROJ _ | TC_APP _ | TC_NVAR _) => SOMEB tc1
                  | _ => (case (lookTc tc1) 
                           of SOME x => SOMEU x
                            | _ => NOTHING))

             val ntc2 = lookTc tc2
          in (case (ntc1, ntc2)
               of (NOTHING, NONE) => NOTHING
                | (SOMEU z1, NONE) => SOMEU (LT.tcc_parrow(z1, tc2))
                | (SOMEB z1, NONE) => SOMEB (tcBox(LT.tcc_parrow(z1, tc2)))
                | (NOTHING, SOME z2) => SOMEU (LT.tcc_parrow(tc1, z2))
                | (SOMEU z1, SOME z2) => SOMEU (LT.tcc_parrow(z1, z2))
                | (SOMEB z1, SOME z2) => SOMEB (tcBox(LT.tcc_parrow(z1, z2))))
         end
     | (TC_APP(tc, ts)) => 
         (case tcUncover tc of NOTHING => NOTHING
                             | _ => SOMEU x)
     | _ => NOTHING)

in (lookTc, ltWrap, tcsWrap)
end

val (tcWrap, ltWrap, tcsWrap) = genWrap false

fun genWrapNEW bbb = 
  let fun tc_wmap (w, u) t =
        (case (tc_out t)
          of (TC_VAR _ | TC_NVAR _) => t
           | TC_PRIM pt => if PT.unboxed pt then LT.tcc_wrap t else t
           | TC_FN (ks, tc) => LT.tcc_fn(ks, w tc) (* impossible case *)
           | TC_APP (tc, tcs) => LT.tcc_app(w tc, map w tcs)
           | TC_SEQ tcs => LT.tcc_seq(map w tcs)
           | TC_PROJ (tc, i) => LT.tcc_proj(w tc, i)
           | TC_SUM tcs => LT.tcc_sum (map w tcs)
           | TC_FIX ((n,tc,ts), i) => 
               LT.tcc_fix((n, tc_norm (u tc), map w ts), i) 

           | TC_TUPLE (_, ts) => LT.tcc_wrap(LT.tcc_tuple (map w ts)) (* ? *)
           | TC_ARROW (FF_VAR(b1,b2), ts1, ts2) =>  
               let val nts1 =    (* too specific ! *)                       
                     (case ts1 of [t11,t12] => [w t11, w t12] 
                                | _ => [w (tc_autotuple ts1)])
                   val nts2 = [w (tc_autotuple ts2)]
                   val nt = LT.tcc_arrow(LT.ffc_fixed, nts1, nts2)
                in if b1 then nt else LT.tcc_wrap nt
               end
           | TC_ARROW (FF_FIXED, _, _) =>  
                bug "unexpected TC_FIXED_ARROW in tc_umap"
           | TC_TOKEN (k, t) => bug "unexpected token tyc in tc_wmap"
           | TC_BOX _ => bug "unexpected TC_BOX in tc_wmap"
           | TC_ABS _ => bug "unexpected TC_ABS in tc_wmap"
           | _ => bug "unexpected other tycs in tc_wmap")

      fun tc_umap (u, w) t =
        (case (tc_out t)
          of (TC_VAR _ | TC_NVAR _ | TC_PRIM _) => t
           | TC_FN (ks, tc) => LT.tcc_fn(ks, u tc) (* impossible case *) 
           | TC_APP (tc, tcs) => LT.tcc_app(u tc, map w tcs)
           | TC_SEQ tcs => LT.tcc_seq(map u tcs)
           | TC_PROJ (tc, i) => LT.tcc_proj(u tc, i)
           | TC_SUM tcs => LT.tcc_sum (map u tcs)
           | TC_FIX ((n,tc,ts), i) => 
               LT.tcc_fix((n, tc_norm (u tc), map w ts), i) 

           | TC_TUPLE (rk, tcs) => LT.tcc_tuple(map u tcs)
           | TC_ARROW (FF_VAR(b1,b2), ts1, ts2) =>  
               LT.tcc_arrow(LT.ffc_fixed, map u ts1, map u ts2)
           | TC_ARROW (FF_FIXED, _, _) =>  
               bug "unexpected TC_FIXED_ARROW in tc_umap"
           | TC_PARROW _ => bug "unexpected TC_PARROW in tc_umap"

           | TC_BOX _ => bug "unexpected TC_BOX in tc_umap"
           | TC_ABS _ => bug "unexpected TC_ABS in tc_umap"
           | TC_TOKEN (k, t) => 
               if token_eq(k, wrap_token) then 
                 bug "unexpected TC_WRAP in tc_umap"
               else tc_inj (TC_TOKEN (k, u t))

           | _ => bug "unexpected other tycs in tc_umap")

      fun lt_umap (tcf, ltf) t = 
        (case (lt_out t)
          of LT_TYC tc => LT.ltc_tyc (tcf tc)
           | LT_STR ts => LT.ltc_str (map ltf ts)
           | LT_FCT (ts1, ts2) => LT.ltc_fct(map ltf ts1, map ltf ts2)
           | LT_POLY (ks, xs) => LT.ltc_poly(ks, map ltf xs)
           | LT_PST its => LT.ltc_pst (map (fn (i, t) => (i, ltf t)) its)
           | LT_CONT _ => bug "unexpected CNTs in lt_umap"
           | LT_IND _ => bug "unexpected INDs in lt_umap"
           | LT_ENV _ => bug "unexpected ENVs in lt_umap")

      val {tc_wmap=tcWrap, tc_umap=tcMap, lt_umap=ltMap, cleanup} =
        LtyDict.wmemo_gen{tc_wmap=tc_wmap, tc_umap=tc_umap, lt_umap=lt_umap}

      fun ltWrap x = 
        LT.ltw_tyc (x, (fn tc => LT.ltc_tyc (tcWrap tc)),
                    fn _ => bug "unexpected case in ltWrap")

   in (tcWrap o tc_norm, ltWrap o lt_norm, 
       tcMap o tc_norm, ltMap o lt_norm, cleanup)
  end

end (* toplevel local *)
end (* structure LtyUtil *)

