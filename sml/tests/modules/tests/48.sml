(* test48.sml *)
(* keywords: functor, datatype *)

signature SIGT =
sig
  type t1
  val e1: t1
end;

functor T (B : sig type key end) : SIGT =
struct
  datatype t1 = EMPTY | TREE of B.key
  val e1 = EMPTY
end;

structure A = T(type key = string);

val x = A.e1;
