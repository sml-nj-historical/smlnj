(* bug76.sml
76. parenthesized infix expression in fun lhs
*)

infix o;
fun (f o g) x = f (g x);
