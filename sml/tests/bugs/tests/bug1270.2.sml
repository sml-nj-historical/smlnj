(* bug1270.2.sml *)

signature T =
sig
  type p
  structure A : sig type s end
  val f1 : unit -> A.s list
end;

functor PD(type t):
sig
  val f2 : (unit -> t) -> t
end = 
struct
  fun f2 f = f()
end

functor TF(structure P : T) : 
sig
  type r = (P.A.s list)
  val g : P.p -> r option
end = 
struct
  type r = P.A.s list
  structure B = PD(type t = r)
  fun g x = SOME(B.f2 P.f1)
end;
