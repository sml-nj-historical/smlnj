(* bug945.sml *)

structure T =
struct
  infix 3 |||
  fun op ||| (x,y) = y
  fun foo n = n || 5
end;

open T;

val x = |||;
