(* bug277.sml *)
(* dbm, 3/16/90 *)

signature S1 =
sig
  type d
end;

functor F(X: S1) :
sig
  datatype a = C of X.d
end =
struct
  datatype a = C of X.d
  val f = fn (x : a) => x
end;


