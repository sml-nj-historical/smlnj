(* bug1386.2.sml *)

signature CAT = 
sig 
  eqtype cat 
  val c : cat
end;

signature L_F =
sig
  eqtype t
  structure Cat : CAT
      where type cat = t
  val r : t
end;

functor Translator
    (structure Cat : CAT
     structure LF : L_F where type t = Cat.c) =
struct
  val w = (Cat.c = LF.r)
end;
