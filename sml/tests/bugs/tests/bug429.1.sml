(* bug429.1.sml *)

signature BB =
sig
  type t
  val x : t
end;

signature CC =
sig
  structure B: BB
end;

signature DD =
sig
  include CC
end;

signature TT =
sig
  structure D: DD
  val y: D.B.t
end;

functor F(X: DD) : TT =
struct
  structure D = X
  val y = D.B.x
end;
