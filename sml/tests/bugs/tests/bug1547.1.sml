(* bug1547.1.sml *)

signature S = sig type t  val x: t  val f: t -> string end;

structure A :> S = struct type t = int  val x = 1 val f = Int.toString end;
structure B :> S = struct type t = string  val x = "1"  fun f s = s end;

signature SS = sig structure X : S = A end;

structure C :> SS = struct structure X = B end;

val x = C.X.f(A.x);
