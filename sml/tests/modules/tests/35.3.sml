(* test35.3.sml *)

type s0 = int -> int;  (* not an equality type *)

signature S =
sig
  eqtype t
  sharing type t = s0  (* should cause an error *)
end;
