(* 223.1.sml *)

signature S = sig end;

funsig FS(structure X: S) =  (* "structure" required *)
sig
  structure C : sig structure A : S = X end
end; (* may be "where A = X" *)

functor F(functor G: FS structure Y: S) =
struct
  structure B = G(structure X = Y)
end;
