(* 285.sml *)

(* t9.sml *)
(* where types and include *)

signature S =
sig
  type t
end;

signature S1 = S where type t = int;

signature S2 =
sig
  include S1
end;

structure A : S2 =
struct
  type t = bool
end;


