(* bug1385.4.sml *)
(* should not elaborate *)

signature CAT = sig datatype t = C end;

signature L_F =
sig
  structure Cat : CAT
end;

signature T1 =
sig
  structure Cat : CAT
  structure LF : L_F 
      where Cat = Cat
end;

signature T2 = 
sig
  structure Cat : CAT
  structure LF : L_F
  structure X : T1 where Cat = Cat
                   where LF = LF
end;
