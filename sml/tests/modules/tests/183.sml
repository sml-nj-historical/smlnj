(* 183.sml *)
(* module tests: datatype replication declarations *)

signature S =
sig
  datatype t = A | B of t
  val test: t -> unit
end;

structure A : S =
struct
  datatype t = A | B of t
  fun test(x: t) = ()
end;

functor F(X: S) =
struct
  datatype s = datatype X.t
  fun f(x: s) = case x of A => true | B y => false
end;
  
structure B = F(A);

val x = B.f(A.A);
