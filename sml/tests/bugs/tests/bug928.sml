(* bug928.sml *)

signature sig0 = sig type t end;

signature sig1 =
sig
  datatype t = T
  include sig0
end;
