(* 222.sml *)
(* tests structure sharing *)

signature S1 =
sig
  eqtype t
  val x : t
end;

funsig FS1(A:S1) =
sig
  structure C : sig structure A : S1 end
  where A = A
end;

functor F(functor G:FS1 structure A:S1) =
struct
  structure B = G(A)
  val y = (A.x = B.C.A.x)
end;

structure a:S1 = struct type t=int val x=5 end;

functor g(A:S1) =
struct
  structure C=struct structure A=A end
end;

structure b=F(functor G=g structure A=a);

b.y;
