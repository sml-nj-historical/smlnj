(* bug1194.1.sml *)

signature SS = 
sig
  type 'a t
end;

functor F(structure X : SS) = 
struct
  datatype p = datatype X.t
end;
