(* 279.sml *)
(* where types and sigmatch *)

signature S =
sig
  type t
end;

signature S1 = S where type t = int;

structure A : S1 =
struct
  type t = int
end;


