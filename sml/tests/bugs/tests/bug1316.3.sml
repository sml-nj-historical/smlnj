(* bug1316.3.sml *)

fun test (a, l, i, NONE) = (a, i, l) 
  | test (a, l, i, SOME n) =
      if (i + n > l) 
      then raise Subscript 
      else (a, i, SOME n);

