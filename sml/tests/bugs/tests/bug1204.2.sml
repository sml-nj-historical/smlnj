(* bug1204.2.sml *)

signature S =
sig
  type t
end;

functor F(X: S where type A.t = int) =
struct end;


