(* bug361.sml *)
(* Problem: Error: Compiler bug: Functor.applyFunctor.insttyc *)

signature AA =
sig
  datatype s  =  a of s
end;  (* signature AA *)

signature BB =
sig
  structure A : AA
end;  (* signature BB *)

signature CC =
sig
  structure B : BB
  type v
end;  (* signature CC *)

functor F (structure B : BB) : CC =
struct
  structure B = B
  structure A = B.A
  open A
  type u = s
end; (* functor F *)

structure C : CC = F (structure B = B);
