(* bug927.sml *)
(* 927. Compiler bug: getSigTSlots caused by include *)

signature sig0 = sig type t end;

signature sig1 =
sig
  datatype t = T
  include sig0 (*sharing type t = t*)
end;

(* Error: Compiler bug: getSigTSlots *)
