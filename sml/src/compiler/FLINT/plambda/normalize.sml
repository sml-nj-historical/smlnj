(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* normalize.sml *)

(* Converting the Standard PLambda.lexp into the A-Normal Form *)

signature NORMLEXP = 
sig 
  val normLexp : PLambda.lexp -> Lambda.lexp

end (* signature SPECIALIZE *)

structure NormLexp : NORMLEXP =
struct 

local structure LT = PLambdaType
      structure DI = DebIndex
      structure PT = PrimTyc
      structure L = Lambda
      structure A = Access
      open Access PLambda 
in

val say = Control.Print.say
fun bug s = ErrorMsg.impossible ("Normalize: " ^ s)
val mkv = LambdaVar.mkLvar
val ident = fn le : L.lexp => le

fun DECON'(dc as (_, A.REF, lt), ts, x) =
      L.APP (L.PRIM (PrimOp.DEREF, LT.lt_swap lt, ts), x)
  | DECON'(dc as (_, A.SUSP(SOME(_, A.LVAR f)), lt), ts, x) = 
      let val v = mkv()
       in L.LET(v, L.TAPP (L.VAR f, ts), L.APP (L.VAR v, x))
      end
  | DECON' z = L.DECON z

fun mksv le = 
  (case le
    of VAR x => L.VAR x
     | INT x => L.INT x
     | INT32 x => L.INT32 x
     | WORD x => L.WORD x
     | WORD32 x => L.WORD32 x
     | REAL x => L.REAL x
     | STRING x => L.STRING x
     | PRIM x => L.PRIM x
     | _ => bug "unexpected lambda expressions in mksv")

fun lpdt {default=e1, table=es} =
  let val (sv1, hdr1) = lpsv e1
      val (vs, hdr2) = 
        foldr (fn ((t,xe), (r, h)) => 
                 let val (sv, nh) = lpsv xe
                  in (case sv
                       of L.VAR w => ((t,w)::r, h o nh)
                        | _ => bug "unexpected cases in lpdt1")
                 end) ([], ident) es
   in (case sv1 of L.VAR v => ({default=v, table=vs}, hdr1 o hdr2)
                 | _ => bug "unexpected cases in lpdt2")
  end


(** val lpsv : PLambda.lexp -> Lambda.value * (Lambda.lexp -> Lambda.lexp) *)
and lpsv le = 
  (case le
    of (VAR _  | INT _ | WORD _ | INT32 _ | WORD32 _) => (mksv le, ident)
     | (REAL _ | STRING _ | PRIM _) => (mksv le, ident)
     | (GENOP(dc, p, lt, ts)) =>
         let val (ndc, hdr) = lpdt dc
          in (L.GENOP(ndc, p, lt, ts), hdr)
         end
     | _ => 
         let val v = mkv()
             val ne = loop le
          in (L.VAR v, fn xe => L.LET(v, ne, xe))
         end)

and lpcon (c, sv) =
  (case c
    of DATAcon (x, ts, v) => 
         let val (nx, hdr) = lpdc x
             val xh = fn z => L.LET(v, DECON'(x, ts, sv), z)
          in (L.DATAcon nx, hdr, xh)
         end
     | INTcon x => (L.INTcon x, ident, ident)
     | INT32con x => (L.INT32con x, ident, ident)
     | WORDcon x  => (L.WORDcon x, ident, ident)
     | WORD32con x => (L.WORD32con x, ident, ident)
     | REALcon x  => (L.REALcon x, ident, ident)
     | STRINGcon x => (L.STRINGcon x, ident, ident)
     | VLENcon x  => (L.VLENcon x, ident, ident))

and lpacc(PATH(acc, i)) = 
      let val (v, h) = lpacc acc
          val w = mkv()
       in (w, fn le => h(L.LET(w, L.SELECT(i, L.VAR v), le)))
      end
  | lpacc(LVAR v) = (v, ident)
  | lpacc _ = bug "unexpected cases in lpacc"

and lpdc (s, EXN (acc as PATH _), t) = 
      let val (nav, hdr) = lpacc acc
       in ((s, EXN (LVAR nav), t), hdr)
      end
  | lpdc dc = (dc, ident)

and lpes es = 
  let fun h (x, (vs, hdr)) = 
        let val (v, nh) = lpsv x
         in (v::vs, nh o hdr)
        end
      val (vs, hdr) = List.foldr h ([], ident) es
   in (vs, hdr)
  end

and loop le = 
  (case le
    of (VAR _  | INT _ | WORD _ | INT32 _ | WORD32 _) => L.SVAL (mksv le)
     | (REAL _ | STRING _ | PRIM _) => L.SVAL (mksv le)
     | GENOP _ => let val (sv, hdr) = lpsv le
                  in hdr(L.SVAL(sv))
                 end
     | TFN (ks, e) => L.TFN(ks, loop e)
     | TAPP (e, ts) => 
         let val (sv, hdr) = lpsv e 
          in hdr(L.TAPP(sv, ts)) 
         end
     | PACK (lt, ts, nts, e) => 
         let val (sv, hdr) = lpsv e 
          in hdr(L.PACK(lt, ts, nts, sv)) 
         end
     | CON (x, ts, e) => 
         let val (sv, hdr) = lpsv e 
             val (nx, hdr2) = lpdc x
          in hdr(hdr2(L.CON(nx, ts, sv)))
         end
(*
     | DECON (x, ts, e) => 
         let val (sv, hdr) = lpsv e 
             val (nx, hdr2) = lpdc x
          in hdr(hdr2(L.DECON(nx, ts, sv)))
         end
*)
     | SWITCH (e, reps, cases, opp) => 
         let val (sv, hdr) = lpsv e
             val (cases', nhdr) = 
               List.foldr (fn ((c,x), (cs, h)) => 
                            let val (nc, nh, xh) = lpcon (c, sv)
                             in ((nc, xh (loop x))::cs, h o nh)
                            end) ([], ident) cases
             val opp' = (case opp of NONE => NONE | SOME x => SOME(loop x))
             fun swi (sv, reps, [(_,xe)], NONE) = xe
               | swi z = L.SWITCH z
          in hdr(nhdr(swi(sv, reps, cases', opp')))
         end
     | FN(v, t, e) => L.FN(v, t, loop e)
     | FIX(vs, ts, es, eb) => L.FIX(vs, ts, map loop es, loop eb)
     | APP(e1, e2) => 
         let val (sv1, hdr1) = lpsv e1
             val (sv2, hdr2) = lpsv e2
          in hdr1 (hdr2 (L.APP(sv1, sv2)))
         end
     | LET(v, e1, e2) => L.LET(v, loop e1, loop e2)
     | RECORD es => 
         let val (vs, hdr) = lpes es
          in hdr(L.RECORD vs)
         end
     | SRECORD es => 
         let val (vs, hdr) = lpes es
          in hdr(L.SRECORD vs)
         end
     | VECTOR (es, t) => 
         let val (vs, hdr) = lpes es
          in hdr(L.VECTOR(vs, t))
         end
     | SELECT (i, e) => 
         let val (sv, hdr) = lpsv e
          in hdr(L.SELECT(i, sv))
         end
     | ETAG (e, t) => 
         let val (sv, hdr) = lpsv e
          in hdr(L.ETAG(sv, t))
         end
     | RAISE (e, t) => 
         let val (sv, hdr) = lpsv e
          in hdr(L.RAISE(sv, t))
         end
     | HANDLE (e1, e2) => 
         let val (sv, hdr) = lpsv e2
          in hdr(L.HANDLE(loop e1, sv))
         end
     | WRAP _ => bug "unexpected WRAP lexp"
     | UNWRAP _ => bug "unexpected UNWRAP lexp")

fun normLexp (lexp as FN(_, _, _)) = loop lexp
  | normLexp _ = bug "unexpected lambda expressions in wrapLexp"

end (* toplevel local *)
end (* structure NormLexp *)

