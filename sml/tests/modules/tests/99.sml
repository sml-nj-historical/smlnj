(* check for duplicate bindings in signatures *)

signature S =
sig
  type t
  datatype d = D
  val x : t
  structure A : sig type x end
  exception e 
  val D : int
  type t
  datatype d = D
  val x : t
  structure A : sig type x end
  exception e of int
end
