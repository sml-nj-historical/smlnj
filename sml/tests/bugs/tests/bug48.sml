(* bug48.sml *)
(*
  A simple identity declaration in the withtype clause of a datatype declaration
  will not be printed properly.
*)

datatype foo = A
withtype t = int;
