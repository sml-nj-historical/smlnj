(* 75.sml *)
(* show that type constructors can escape from a functor body *)

signature S =
sig
  type t
end;

functor F() : S =
struct
  datatype d = D
  type t = d * d
end;
