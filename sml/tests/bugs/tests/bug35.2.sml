(* bug35.2.sml *)
(* compiler error abstractType *)

signature FORMULA =
sig
  type formula
  val NUM : formula
end;

functor Parse(F : FORMULA) = 
struct

   fun parse() : F.formula = (0, F.NUM)
(*  val parse : unit -> F.formula = (fn () => (0, F.NUM))  -or-
    val parse : F.formula = (0, F.NUM) -- don't cause abstractType error *)

end;
