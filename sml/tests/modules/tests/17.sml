(* test17.sml *)
(* keywords: functor, open *)

(* derived from new mlyacc *)

signature S1 =
sig
  val x : unit
  datatype t = K
end;

signature S2 =
sig
  structure C : S1
  val foo : C.t -> unit
end;

functor mkGraph(structure C : S1
		structure U : S2
		sharing U.C = C)  =
struct
  open C
  open U 
end;
