structure PFlatten : PFLATTEN =
struct

local structure LT = PLambdaType
      structure LV = LambdaVar
      structure F = FLINT
in

    type llty = PLambda.lty
    type ltyc = PLambda.tyc
    type flty = FLINT.lty
    type ftyc = FLINT.tyc
    type lexp = FLINT.lexp
    type value = FLINT.value
    type lvar = FLINT.lvar

val mkv = LambdaVar.mkLvar

(* all_flatten: lty -> (lty list * bool * unflattenfn * flattenfn)
 *   `lty list': the flattened types
 *   `bool': whether the `lty list' is raw
 * unflattenfn: (lvar * lexp) -> (lvar list * lexp)
 *   turn `lexp' from an expression expecting a single value bound to `lvar'
 *   to an expression expecting multiple values to be bound to `lvar list'.
 *   It seems generally more convenient to choose the `lvar list' inside
 *   bundlefn than outside.
 * flattenfn: value -> (value list * (lexp -> lexp))
 *   expand `value' into its flattened `value list' around `lexp'.
 *   The `value list' might be required in order to construct the
 *   `lexp' argument, which explains the fact that `value' and `lexp'
 *   are passed in two steps. *)

fun all_flatten lty = 
  let val (raw, ltys, flag) = LT.lt_autoflat lty
   in if flag then
        (ltys, raw,
 	 fn (lv, lexp) => 
 	   let val lvs = map (fn _ => mkv()) ltys 
 	    in (lvs, F.RECORD(F.RK_RECORD, map F.VAR lvs, lv, lexp)) 
 	   end, 
 	 fn v => 
 	   let val lvs = map (fn _ => mkv()) ltys 
 	    in (map (fn v => F.VAR v) lvs, 
 	        fn lexp => 
 		   #1 (foldl (fn (lv, (lexp, field)) => 
 		        (F.SELECT(v, field, lv, lexp), field+1)) 
 		          (lexp, 0) 
 			    lvs)) 
           end) 
      else 
        (ltys, raw, 
  	 fn (lv, lexp) => ([lv], lexp), 
 	 fn v => ([v], fn lexp => lexp)) 
  end

(*
  if !Control.CG.misc4 = 1998 then
     ([lty], true, fn (lv, lexp) => ([lv], lexp), 
      fn v => ([v], fn lexp => lexp)) 
  else
*)
     

fun ltc_flat    lty = #1 (all_flatten lty)
fun ltp_flat    lty = #2 (all_flatten lty)
fun v_unflatten lty = #3 (all_flatten lty)
fun v_flatten   lty = #4 (all_flatten lty)

fun ltc_raw x = x
fun tcc_raw x = x

end


end
