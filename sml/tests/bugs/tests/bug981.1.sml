(* bug981.1.sml *)

signature S =
sig
  datatype d = A of int*int
end;

structure X :> S =
struct
  datatype d = A of int*int
end;
