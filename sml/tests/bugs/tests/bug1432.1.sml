(* bug1432.1.sml *)

signature S1 =
sig
  datatype dt = DT
end;

signature S2 =
sig
  structure NestedS1 : S1
end;

structure GlobalS1 =
struct
  datatype dt = DT
end;

functor F (X : S2 where type NestedS1.dt = GlobalS1.dt) =
struct
  structure MyS1 : S1 = X.NestedS1 (* match against S1 necessary *)
end;
