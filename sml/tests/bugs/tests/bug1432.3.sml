(* bug1432.3.sml *)

signature S1 =
sig
  datatype d = D
end;

structure GlobalS1 =
struct
  datatype d = D
end;

signature S2 =
sig
  structure N : S1 = GlobalS1
end;

functor F (X : S2) =
struct
  structure MyS1 : S1 = X.N
end;
