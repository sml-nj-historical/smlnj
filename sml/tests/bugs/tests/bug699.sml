(* bug699.sml *)
(* 699. "Compiler bug: ModuleUtil: lookFormalBinding 1" (secondary) *)

functor F(
  structure A : sig end
	and B : SIGU
  sharing A = B.C) =
struct
end;
