(* 187.sml *)

signature S =
sig
  type s
  datatype d = D of s
  eqtype t
  sharing type d = t
end;

functor F(X: S) =
struct
  fun f (x:X.d) = (x = x)
end;
