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

(** type convertion; used by the reify phase *)
val tkLty : tkind -> lty

(** used by the ltNarrow phase *)
val narrowGen : unit -> ((tyc -> tyc) * (lty -> lty) * (unit -> unit))

end 

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

(*
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
                            (let fun h(z, NONE) = z
                                   | h(_, SOME z) = z
                                 val nt = LT.tcc_tuple(ListPair.map h (ts, nts))
                              in SOMEU nt
                             end)
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
*)

fun genWrap bbb = 
  let fun tcWrap t = 
        let val nt = LtyKernel.tcc_wrap t
         in if LT.tc_eqv(nt,t) then NONE
            else SOME nt
        end

      and tcsWrap xs = 
        let fun p([], flag, bs) = if flag then SOME(rev bs) else NONE
              | p(a::r, flag, bs) = 
                  (case (tcWrap a) of NONE => p(r, flag, a::bs)
                                    | SOME z => p(r, true, z::bs))
         in p(xs, false, [])
        end

      and ltWrap x = 
        (case lt_out x
          of LT_TYC t => (case tcWrap t
                           of NONE => NONE
                            | SOME z => SOME(LT.ltc_tyc z))
           | _ => bug "unexpected case in ltWrap")
   in (tcWrap, ltWrap, tcsWrap)
  end

val (tcWrap, ltWrap, tcsWrap) = genWrap false

(** val tkLty : tkind -> lty *)
fun tkLty tk = 
  (case tk_out tk
    of TK_MONO => LT.ltc_int
     | TK_BOX => LT.ltc_int
     | TK_SEQ ks => LT.ltc_tuple (map tkLty ks)
     | TK_FUN (ks, k) => LT.ltc_parrow(LT.ltc_tuple(map tkLty ks), tkLty k))

fun tcNarrow t = 
  (case (tc_out t)
    of TC_PRIM pt => 
         if PT.isvoid pt then LT.tcc_void else t
     | TC_TUPLE (_, tcs) => LT.tcc_tuple (map tcNarrow tcs)
     | TC_ARROW (r, ts1, ts2) => 
         LT.tcc_arrow(r, map tcNarrow ts1, map tcNarrow ts2)
     | _ => LT.tcc_void)

fun ltNarrow t = 
  (case lt_out t
    of LT_TYC tc => LT.ltc_tyc (tcNarrow tc)
     | LT_STR ts => LT.ltc_str (map ltNarrow ts)
     | LT_PST its => LT.ltc_pst (map (fn (i, t) => (i, ltNarrow t)) its)
     | LT_FCT (ts1, ts2) => LT.ltc_fct(map ltNarrow ts1, map ltNarrow ts2)
     | LT_POLY (ks, [x]) => LT.ltc_fct([LT.ltc_str (map tkLty ks)], 
                                      [ltNarrow x])
     | LT_POLY (ks, _) => bug "unexpectd POLYs in ltNarrow"
     | LT_CONT _ => bug "unexpected CNTs in ltNarrow"
     | LT_IND _ => bug "unexpected INDs in ltNarrow"
     | LT_ENV _ => bug "unexpected ENVs in ltNarrow")

fun tcNarrowSt t = 
  let val nt = tc_whnm t
   in (case tc_out nt
        of TC_PRIM pt => 
             if PT.isvoid pt then LT.tcc_void else nt
         | TC_TUPLE (_, tcs) => LT.tcc_tuple (map tcNarrowSt tcs)
         | TC_ARROW (r, ts1, ts2) => 
             LT.tcc_arrow(r, map tcNarrowSt ts1, map tcNarrowSt ts2)
         | _ => LT.tcc_void)
  end

fun ltNarrowSt t = 
  (case lt_out (lt_whnm t)
    of LT_TYC tc => LT.ltc_tyc (tcNarrowSt tc)
     | LT_STR ts => LT.ltc_str (map ltNarrowSt ts)
     | LT_PST its => LT.ltc_pst (map (fn (i, t) => (i, ltNarrowSt t)) its)
     | LT_FCT (ts1, ts2) => LT.ltc_fct(map ltNarrowSt ts1, map ltNarrowSt ts2)
     | LT_POLY (ks, [x]) => LT.ltc_fct([LT.ltc_str (map tkLty ks)], 
                                       [ltNarrowSt x])
     | LT_POLY (ks, _) => bug "unexpectd POLYs in ltNarrowSt"
     | LT_CONT _ => bug "unexpected CNTs in ltNarrowSt"
     | LT_IND _ => bug "unexpected INDs in ltNarrowSt"
     | LT_ENV _ => bug "unexpected ENVs in ltNarrowSt")

(* val narrowGen : unit -> ((tyc -> tyc) * (lty -> lty) * (unit -> unit)) *)
fun narrowGen ()
 = let val m1 = ref (TcDict.mkDict())
       val m2 = ref (LtDict.mkDict())
       fun lookTc t = 
         let val u = !m1
          in (case TcDict.peek(u, t)
               of SOME t' => t'
                | NONE => 
                    let val x = tcN t
                        val _ = (m1 := TcDict.insert(u, t, x))
                     in x
                    end)
         end

       and lookLt t = 
         let val u = !m2
          in (case LtDict.peek(u, t)
               of SOME t' => t'
                | NONE => 
                    let val x = ltN t
                        val _ = (m2 := LtDict.insert(u, t, x))
                     in x
                    end)
         end

       and tcN t = 
         (case (tc_out t)
           of TC_PRIM pt => 
                if PT.isvoid pt then LT.tcc_void else t
            | TC_TUPLE (_, tcs) => LT.tcc_tuple (map lookTc tcs)
            | TC_ARROW (r, ts1, ts2) => 
                 LT.tcc_arrow(r, map lookTc ts1, map lookTc ts2)
            | _ => LT.tcc_void)

       and ltN t = 
         (case (lt_out t)
           of LT_TYC tc => LT.ltc_tyc (tcN tc)
            | LT_STR ts => LT.ltc_str (map lookLt ts)
            | LT_PST its => LT.ltc_pst (map (fn (i, t) => (i, lookLt t)) its)
            | LT_FCT (ts1, ts2) => LT.ltc_fct(map lookLt ts1, map lookLt ts2)
            | LT_POLY (ks, [x]) => LT.ltc_fct([LT.ltc_str (map tkLty ks)], 
                                              [lookLt x])
            | _ => bug "unexpected ltys in ltNarrow")

    in (lookTc, lookLt, fn () => ())
   end

end (* toplevel local *)
end (* structure LtyUtil *)

