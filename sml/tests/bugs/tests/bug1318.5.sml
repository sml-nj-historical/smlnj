(* bug1318.5.sml *)

funsig FSIG() = 
sig
  type a
  type b
  val f : a -> b
end;

functor FUN(functor F : FSIG) = 
struct
  structure S = F()
  type a = S.a
  type b = S.b
  val g = S.f : S.a -> S.b
end;

functor F0 () =
struct
  type a = bool
  type b = int
  fun f(x:a) : b = 3
end;

structure A = FUN(functor F = F0);

A.g true : int;
