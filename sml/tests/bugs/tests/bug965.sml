(* bug965.sml *)

functor F (datatype a = A of int * int) = struct end;
structure S = F(datatype a = A of int);
