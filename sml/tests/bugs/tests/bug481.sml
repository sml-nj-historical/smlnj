(* bug481.sml *)

signature S = sig
  datatype a = A | B of string
  datatype b = B | C
end;

functor F(S:S) =
struct
  open S

  fun matchA A = true
    | matchA _ = false

  fun matchB B = true
    | matchB _ = false

  fun matchC C = true
    | matchC _ = false
end;

structure S = F (datatype a = A | B of string
		 datatype b = B | C );
