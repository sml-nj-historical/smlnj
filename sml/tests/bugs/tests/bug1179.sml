(* bug1179.sml *)

structure A : sig type 'a t val f : 'a -> 'a t end =
struct
  fun f y = [y]  (* missing type t *)
end;

