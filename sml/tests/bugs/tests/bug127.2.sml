(* bug127.2.sml *)
(* sharing and equality propagation *)

signature SIG =
sig
  structure S : sig type t val x: t end
  structure T : sig eqtype t end
  sharing S = T
end;

functor f(X:SIG) = struct val b:bool = X.S.x = X.S.x end;
