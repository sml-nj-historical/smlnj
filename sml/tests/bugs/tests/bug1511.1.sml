(* bug1511.1.sml *)

signature S =
sig
  type ('a, 'b) t = 'a -> 'b
  val x: ('a, int) t
end;

structure M :> S =
struct
  type ('a, 'b) t = 'a -> 'a
  fun x y = y
end;
