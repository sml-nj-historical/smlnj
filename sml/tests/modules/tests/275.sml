(* 275.sml *)
(* sharing and where type *)

signature S =
sig
  type s
  type t
  sharing type s = t
end;

(* S1 should succeed *)
signature S1 = S where type t = int * int;

