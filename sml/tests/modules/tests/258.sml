(* bj3.sml *)

signature S =
sig
  type t 
  val x : t
end;

functor F(type r): sig val f : r -> r end = 
struct
  fun f z = z
end;

functor G(X : S) : S = 
struct
  structure B = F(type r = X.t)
  type t = X.t -> X.t
  val x = B.f 
end;

