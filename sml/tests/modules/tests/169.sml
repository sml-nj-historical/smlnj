(* a6.sml -- ok *)

signature S =
sig
  datatype t = U
  withtype u = t
  type v = u
end;

structure A : S =
struct
  datatype t = U
  withtype u = t
  type v = u
end;

