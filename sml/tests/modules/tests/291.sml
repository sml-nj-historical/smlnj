(* 291.sml *)
(* include <sigexp> *)

signature S1 =
sig
  type t
end;

signature S2 =
sig
  datatype s = C
  include S1 where type t = s
end;
