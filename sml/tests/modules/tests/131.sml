(* test131.sml *)
(* check type sharing in Instantiate (getRep, compatible *)

signature TOK =
sig
  datatype t = TOK
end;

signature LEX =
sig
  type t
  val x : t
end;

signature P =
sig
  structure T : TOK
  val y : T.t
end;

functor F(structure X : LEX
	  structure T: TOK
	  sharing type X.t = T.t) 
        : P =
struct
    structure T = T
    val y = X.x
end;
