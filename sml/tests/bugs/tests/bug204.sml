(* bug204.sml *)
(* constructors A and B should not be printed in signature of structure X *)

signature S1 = sig type d end;
structure X : S1 = struct datatype d = A | B of int end;
