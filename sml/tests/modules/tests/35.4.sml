(* test35.4.sml *)

type s0 = int -> int;  (* not an equality type *)

signature S =
sig
  eqtype v
  type t
  type u = t * t
  sharing type t = s0  (* should cause an error *)
  sharing type u = v
end;
