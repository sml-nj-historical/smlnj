(* 223.2.sml *)

signature S = sig type t end;

funsig FS(structure X: S) =
sig
  structure C : sig structure A : sig type t = X.t end end
end;

functor F(functor G: FS structure Y: S) =
struct
  structure B = G(structure X = Y)
end;
