(* bug1259.1.sml *)

signature SIG =
sig
  type b'
  datatype a = A1 | A2 of b'
  datatype b = B1 | B2 of a
  sharing type b = b'
end
