(* bug812.sml *)

fun how_big(x:real):unit*unit*real =
  if Real.isNormal x then 
    (print(Real.toString x), print "\n", #3(how_big(2.0*x)))
  else
    raise Overflow;


how_big 2.0; (* should give largest value then "uncaught exception Overflow"*) 

