(* bug1102.sml *)

signature S =
sig
  eqtype s
  datatype t = C of int -> int
  sharing type s = t
end;

functor F(X: S) = struct end;
