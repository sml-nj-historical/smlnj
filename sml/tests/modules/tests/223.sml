(* 223.sml *)
(* same as test1 but with an additional layer of structure *)

signature S =
sig
  eqtype t
  val x : t
end;

funsig FS(structure X: S) =
sig
  structure C : sig structure A : S end
  where A = X
end;

functor F(functor G: FS structure Y: S) =
struct
  structure B = G(structure X = Y)
  val y = (Y.x = B.C.A.x)
end;

structure a:S = struct type t=int val x=5 end;

functor g(structure X: S) =
struct
  structure C = struct structure A = X end
end;

structure D = F(functor G=g structure Y=a);

D.y;
