(* bug1203.sml *)

signature S =
sig
  type t
end;

structure A : S =
struct
  type t = int
end;

functor F1(X: sig type t = A.t end) =
struct
  val x : X.t = 3
end;

functor F2(X: sig structure B: S = A end) =
struct
  val x : X.B.t = 3
end;
