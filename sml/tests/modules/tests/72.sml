(* test72.sml *)

signature S =
sig
  type 'a t
  val x : 'a -> 'a
end;

structure A: S =
struct
  type 'a t = 'a
  val x : 'a -> 'a t t = (fn z => z)
end;
