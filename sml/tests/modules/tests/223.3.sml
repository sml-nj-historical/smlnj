(* 223.3.sml *)

signature S = sig type t end;

funsig FS(structure X: S) =
sig
  structure C : sig structure A : S end
                where type A.t = X.t
end;

functor F(functor G: FS structure Y: S) =
struct
  structure B = G(structure X = Y)
end;
