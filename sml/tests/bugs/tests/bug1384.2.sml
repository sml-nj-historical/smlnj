(* bug1384.2.sml *)
(* should elaborate *)

signature CAT = sig datatype t = C end;

signature L_F =
sig
  structure Cat : CAT
end;

signature TRANSLATOR =
sig
  structure Cat : CAT
  structure LF : L_F 
      where Cat = Cat
end;

functor Translator (structure Cat : CAT)
	  :> TRANSLATOR where Cat = Cat
=
struct
  structure Cat = Cat
  structure LF = struct structure Cat = Cat end
end;
