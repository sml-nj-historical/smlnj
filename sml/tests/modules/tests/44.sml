(* test44.sml *)
(* keywords: equality, signature, functor *)

(* tests pass 4 of eqAnalyze, in case where depend(s) = [] *)

structure A =
struct
  datatype a = mka of int -> int
end;

signature SIG = sig
  datatype d = D of A.a
end;

functor F(X:SIG) =
struct
  val _ = X.D(A.mka(fn x => x)) = X.D(A.mka(fn x => x))
end;
