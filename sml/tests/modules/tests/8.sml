(* test8.sml *)
(* keywords: functor, sharing *)

(* from Tarditi -- tests sharing in unconstrained body structure *)

functor F() = 
struct
  structure M=
  struct
    datatype s = V
  end

  structure C =
  struct
    structure M = M
  end
end;

structure S = F();

val x = S.M.V;  (* caused Subscript exception *)
