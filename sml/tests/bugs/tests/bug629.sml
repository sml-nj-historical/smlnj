(* bug629.sml *)
(* fun declaration syntax *)

fun foo 1 x = 17
  | foo (a, b) = a + b;

fun bar (1, x) = 17
  | bar a b = a - b;

fun splat a b = 4
  | splat x = 5;
