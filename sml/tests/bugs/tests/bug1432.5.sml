(* bug1432.5.sml *)

(* ok *)

signature S1 =
sig
  datatype d = D
end;

signature S1' =
sig
  datatype d = D
  type t
end;

structure GlobalS1 : S1' =
struct
  datatype d = D
  type t = int
end;

signature S2 =
sig
  structure N : S1 = GlobalS1
end;

functor F (X : S2) =
struct
  structure MyS1 : S1 = X.N
end;
