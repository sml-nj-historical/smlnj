(* 186.sml *)

signature S =
sig
  datatype d = D of int -> int
  eqtype t
  sharing type d = t
end;

functor F(X: S) =
struct
  fun f (x:X.d) = (x = x)
end;
