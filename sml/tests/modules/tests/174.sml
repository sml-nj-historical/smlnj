(* 174.sml *)
(* SML96: inconsistent equality props in sharing *)

signature S =
sig
  eqtype t
  datatype d = C of int -> int
  sharing type t = d
end;

functor F(X: S) =
struct
  fun dec(x: int) = x-1
  fun inc(x: int) = x+1
  val x = (X.C dec = X.C inc)
end;
