(* bug127.1.sml *)
(* sharing and equality propagation *)

functor f(structure S : sig type t val x: t end
	  structure T : sig eqtype t end
	  sharing S = T
	 )=
struct val b:bool = S.x = S.x
end;
