(* 289.sml *)
(* include <sigexp> *)

signature S1 =
sig
  type t
end;

signature S2 =
sig
  include S1 where type t = int
end;
