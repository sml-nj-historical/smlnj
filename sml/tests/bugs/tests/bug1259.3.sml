(* bug1259.3.sml *)

signature SIG =
sig
  type a'
  datatype a = A1 | A2 of a'
  sharing type a = a'
end
