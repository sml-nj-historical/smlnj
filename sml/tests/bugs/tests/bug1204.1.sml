(* bug1204.1.sml *)

signature S =
sig
  type t
end;

signature S1 = S where type A.t = int;

