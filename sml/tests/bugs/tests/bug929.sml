(* bug929.sml *)

signature sig0 = sig type t end;

signature sig1 =
sig
  include sig0
  datatype t = T
end;
