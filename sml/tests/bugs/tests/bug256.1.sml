(* bug256.1.sml *)
(* bad function definition causes Compiler bug: r_o in mcopt (0.60) *)

datatype 'a stack = empty | stk of 'a *'a stack;
fun pop stk(e,s) = s;
