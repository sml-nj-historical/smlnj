(* bug1046.sml *)

signature S =
sig
  type s
  type t
  sharing type s = t
end;

signature T = 
sig
  include S
  val x : s
  val y : t -> int
end;

functor F (X: T) =
struct
  val _ = X.y(X.x)
end;
