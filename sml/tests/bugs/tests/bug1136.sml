(* bug1136.sml *)

signature TS1 = 
sig
  eqtype t
  val f : t -> t
end;

signature TS2 = 
sig
  type t
end;

functor SF(structure T1:TS1
	   and T2 : TS2
	   sharing type T1.t = T2.t) = 
struct
  fun test t = T1.f t = t
  fun test ts = List.map T1.f ts = ts 
end;
