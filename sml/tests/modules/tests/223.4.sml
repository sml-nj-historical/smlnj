(* 223.4.sml *)

signature S = sig end;

funsig FS(structure X: S) = sig end;

functor F(functor G: FS) =
struct
  structure B = G(structure X = struct end)
end;

functor G0(structure Z: S) =
struct
  structure C = Z
end;

structure E = F(functor G = G0);
