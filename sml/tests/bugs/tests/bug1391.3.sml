(* bug1391.3.sml *)

signature SIG =
sig
  structure A : sig datatype ('a,'b) dt = C end
  datatype dt = datatype A.dt
end;

structure S :> SIG =
struct
  structure A = struct datatype ('a,'b) dt = C end
  datatype dt = datatype A.dt
end;

S.C : (unit,unit) S.dt;
