(* bug171.sml *)
(* duplicate constructor names in datatype declaration *)

datatype t = C1 | C1 of int
