(* bug1194.2.sml *)

signature SS = 
sig
  datatype 'a t = K
end;

functor F(structure X : SS) = 
struct
  datatype p = datatype X.t
end;
