(* bug1432.4.sml *)

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
  structure N : S1
end;

functor F (X : S2 where N = GlobalS1) =
struct
  structure MyS1 : S1 = X.N
  val b = (MyS1.D = GlobalS1.D)
end;
