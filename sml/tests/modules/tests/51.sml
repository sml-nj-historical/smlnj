(* test51.sml *)
(* keywords: functor, datatype *)

functor F(X: sig datatype d = A end) =
struct
  structure S : sig datatype p = P of X.d end =
    struct datatype p = P of X.d end
end;
