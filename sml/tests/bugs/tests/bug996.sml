(* bug996.sml *)
(* 996. sharing constraints and datatypes *)

signature DERIVED =
sig
(* this doesn't work. *)
  type new_name
  datatype dtype = Constructor of int
(* this does
  datatype dtype = Constructor of int
  type new_name
*)
  sharing type dtype = new_name
end;

functor F1 (structure Derived: DERIVED): DERIVED =
struct
  structure Subtyped = Derived
  open Subtyped
end;

functor F2 (structure Derived: DERIVED): DERIVED =
struct
  structure New = F1 (structure Derived = Derived)
  open New
end;
