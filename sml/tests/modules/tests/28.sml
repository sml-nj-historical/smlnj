(* test28.sml *)
(* keywords: functor, open *)

signature SS1 =
sig
  datatype d = D of int
end;

functor F2(X : SS1) =
struct
  val x = X.D 3
end;

functor G2(Y : sig structure A : SS1 end) =
struct
  structure A = F2(Y.A)
  open A
  open Y
  val y = let val A.D v = x in v end
end;
