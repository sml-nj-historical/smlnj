(* bug32.sml *)

datatype A = B | C of A ref
val x = C(ref B);
val C y = x;
y := x;
x;
