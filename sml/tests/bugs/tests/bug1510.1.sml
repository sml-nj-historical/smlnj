(* bug1510.1.sml *)

functor Cast(type from type to) :>
sig
  type ('a, 'b) t = 'a -> 'b
  val cast: (from, to) t
end =
struct
  type ('a, 'b) t = 'a -> 'a
  fun cast x = x
end;

structure IntListToIntOption =
  Cast(type from = int list
       type to = int option);

val it = IntListToIntOption.cast [17];
