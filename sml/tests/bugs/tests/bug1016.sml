(* bug1016.sml *)

signature S =
sig
  type t = int * int
  datatype a = J of t | K of int
end;

structure H : S =
struct
  type t = int * int
  datatype a = J of t | K of int
end;
