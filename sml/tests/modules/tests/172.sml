signature S =
sig
  eqtype s
  datatype t = C of int -> int
  sharing type s = t
end;
