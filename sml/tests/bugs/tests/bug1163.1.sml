(* bug1163.1.sml *)

datatype t = A of int | B of int;

fun f (a,b) =
    case (a,b)
      of (A i1, (B i2 | A i2)) => i1 + i2
       | ((A i1 | B i1), A i2) => i1 - i2
       | (B i1, B i2) => i1 * i2;
