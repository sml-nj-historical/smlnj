(* test43.sml *)
(* keywords: equality, signature *)

(* tests pass 4 of eqAnalyze, in case where depend(s) = [] *)

structure A =
struct
  datatype a = mka of int -> int
end;

signature SIG = sig
  datatype d = D of A.a
end;
