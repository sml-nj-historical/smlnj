(* bug994.sml *)

val maxint = 4503599627370496.0; (* This is the IEEE double-precision *)
fun realround x = if x>=0.0 then x+maxint-maxint else x-maxint+maxint;

(realround 0.1, realround 0.5, realround 0.7, realround 1.0, realround 1.1);

maxint - 4.503E15;
