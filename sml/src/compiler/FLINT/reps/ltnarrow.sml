(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* ltnarrow.sml *)

signature LTNARROW = 
sig
  val narrow : Lambda.lexp -> Lambda.lexp
end (* signature LTYNARROW *)

structure LtNarrow : LTNARROW = 
struct

local structure LU = LtyUtil
      open Lambda
in

fun bug s = ErrorMsg.impossible ("LtNarrow: " ^ s)

fun narrow lexp = 
let

val (tcNarrow, ltNarrow, clear) = LU.narrowGen ()

fun lpsv sv = 
  (case sv
    of VAR v => sv
     | (INT _ | WORD _ | INT32 _ | WORD32 _ | REAL _ | STRING _) => sv
     | PRIM (p, t, []) => PRIM(p, ltNarrow t, [])
     | PRIM _ => bug "unexpected PRIM in loop"
     | GENOP _ => bug "unexpected GENOP in loop")

fun loop le = 
  (case le
    of SVAL sv => SVAL(lpsv sv)
     | TFN (ks, e) => bug "unexpected TFN in loop"
     | TAPP (v, ts) => bug "unexpected TAPP in loop"

     | WRAP(tc, b, v) => WRAP(tcNarrow tc, b, lpsv v)
     | UNWRAP(tc, b, v) => UNWRAP(tcNarrow tc, b, lpsv v)

     | CON _ => bug "unexpected CON in loop"
     | DECON _ => bug "unexpected CON in loop"

     | SWITCH (v, reps, cases, opp) => 
         let fun g (c, x) = (c, loop x)
             fun h (NONE) = NONE
               | h (SOME x) = SOME(loop x)
          in SWITCH(lpsv v, reps, map g cases, h opp)
         end

     | FN(v, t, e) => FN(v, ltNarrow t, loop e)
     | FIX(vs, ts, es, eb) => 
         FIX(vs, map ltNarrow ts, map loop es, loop eb)
     | APP(v1, v2) => APP(lpsv v1, lpsv v2)
     | LET(v, e1, e2) => LET(v, loop e1, loop e2)
     | RECORD vs => RECORD(map lpsv vs)
     | SRECORD vs => SRECORD(map lpsv vs)
     | VECTOR (vs, t) => VECTOR(map lpsv vs, tcNarrow t)   
     | SELECT (i, v) => SELECT(i, lpsv v)

     | ETAG (v, t) => ETAG(lpsv v, ltNarrow t) 
     | RAISE (v, t) => RAISE(lpsv v, ltNarrow t)       
     | HANDLE (e, v) => HANDLE(loop e, lpsv v)

     | PACK _ => bug "unexpected PACK lexp in ltyComp")

val le = loop lexp
val _ = clear()
in le
end (* function narrow *)
end (* toplevel local *)
end (* structure LtNarrow *)


