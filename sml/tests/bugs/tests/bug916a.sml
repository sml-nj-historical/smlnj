(* bug916a.sml *)
(* 916. Compiler bug: Misc.checkbound from type abbrev in sig *)
(* additional example supplied by Emden Gansner *)

signature s =
sig
  type 'a list_item = string * 'a
end;
