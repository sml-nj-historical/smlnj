(* bug1421.1.sml *)

signature E =
sig
  type 'a err
  val OK : 'a -> 'a err
  val map : ('a -> 'b) -> 'a err -> 'b err
end;

structure E1 : E =
struct
  type 'a err = 'a
  fun OK x = x
  fun map1 f = f
  val map = map1 (* causes an error ... *)
end;
