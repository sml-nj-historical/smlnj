(* bug1263.sml *)

let val a = Math.sqrt(Real.minNormalPos * 0.1)
 in [Real.minNormalPos*0.1, a*a]
end;
