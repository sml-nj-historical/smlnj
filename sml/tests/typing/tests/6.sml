(* 6.sml *)
(* typechecking test *)
(* following should cause error because of failure to generalize
 * explicit type variable 'a *)

val x : 'a list = rev [];
