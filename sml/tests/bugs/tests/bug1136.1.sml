(* bug1136.1.sml *)

signature TS1 = 
sig
  eqtype t
  val f : t -> t
end;

functor SF(structure T1:TS1) =
struct
  fun test t = T1.f t = t
  fun test ts = List.map T1.f ts = ts 
end;
