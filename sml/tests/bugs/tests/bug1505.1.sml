(* bug1505.1.sml *)

signature S =
sig
  datatype t = C
  functor F (val v : t) : sig end
end;

structure S :> S = struct
  datatype t = C
  functor F (val v : t) = struct end
end;
