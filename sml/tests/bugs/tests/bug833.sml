(* bug833.sml *)
(* 833. Compiler bug: getSymbols *)

signature S =
sig
  datatype dt = foo
end;

functor I(X:S) = X;

structure A =
struct 
  datatype dt = foo
end

functor R(functor F(X:S):S) (Y:S)= struct structure C = F(Y) end;

functor P = R(functor F = I);

structure B = P(A);
open B;
open C;
