(* Copyright (c) 1997 YALE FLINT PROJECT *)
(* plambdatype.sml *)

structure PLambdaType : PLAMBDATYPE = 
struct

open LtyExtern

fun bug msg = ErrorMsg.impossible("PLambdaType: "^msg)

(* lt_merge is used by the translate.sml only *)
fun lt_merge(t1, t2) = 
  let fun h ([], []) = []
        | h (x, []) = x
        | h ([], y) = y
        | h (x as ((i,t)::l), y as ((j,s)::r)) = 
             if i < j then ((i,t)::(h(l,y)))
             else if i > j then ((j,s)::(h(x,r)))
                  else ((i, lt_merge(t,s))::(h(l,r)))
   in ltw_pst(t1, 
              (fn ts1 => 
                ltw_pst(t2, fn ts2 => ltc_pst(h(ts1, ts2)), fn _ => t2)), 
              (* 
               * if lt_eqv(t1, t2) then t2 
               * else bug "incompatible PST and STR types in lt_merge"
               *)
              (fn t1 => 
                ltw_pst(t2, fn _ => t1, fn _ => t1))
              (*
               * if lt_eqv(t1, t2) then t1 
               * else bug "incompatible STR and PST types in lt_merge"
               *))
  end (* function lt_merge *)

end (* structure PLambdaType *)

