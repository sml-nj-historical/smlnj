(* bug1432.2.sml *)

signature S1 =
sig
  datatype dt = DT
end;

structure GlobalS1 =
struct
  datatype dt = DT
end;

functor F (X : sig
	         structure NestedS1 : sig
	           datatype dt = datatype GlobalS1.dt
		 end
	       end) =
struct
  structure MyS1 : S1 = X.NestedS1 (* match against S1 necessary *)
end;
