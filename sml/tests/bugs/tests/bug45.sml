(* bug45.sml *)
(*
  Compiling equality for a trivial recursive datatype causes the compiler
  to loop in Equal.equal.(test).
*)

datatype t = A of t;
fun f(x:t) = (x=x);
