(* a4.sml -- fails *)

signature S =
sig
  datatype t = U
  withtype u = t
  and v = u
end;

structure A : S =
struct
  datatype t = U
  withtype u = t
  and v = u
end;

