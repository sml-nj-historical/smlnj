(* 224.sml *)

signature S1 =
sig
  eqtype t
  val x : t
end;

funsig FS1(A:S1) =
sig
  structure C : sig structure A : S1 end
end;

functor F(functor G:FS1 structure A:S1) =
struct
  structure B = G(A)
  val y = (A.x = B.C.A.x)
end;
