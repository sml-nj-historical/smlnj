(* bug1391.2.sml *)

signature SIG =
sig
  structure A : sig datatype ('a,'b) dt = C end
  datatype dt = datatype A.dt
end;

functor Fun (X : SIG) =
struct
  val _ = X.C : (unit,unit) X.dt
end;
