(* bug1391.1.sml *)

signature SIG1 =
sig
  datatype ('a,'b) dt = C of ('a,'b) dt
end;

signature SIG2 =
sig
  structure Struct1 : SIG1
  datatype dt = datatype Struct1.dt   (* datatype replication *)
end;

signature SIG3 =
sig
  structure Struct2 : SIG2
  val f : (unit, unit) Struct2.Struct1.dt -> unit   (* OK! *)
(*
  val f : (unit, unit) Struct2.dt -> unit           (* Problem! *)
*)
end;

functor Fun (structure Struct2 : SIG2) : SIG3 =
struct
  structure Struct2 = Struct2
(*
  fun f (Struct2.Struct1.C _) = ()   (* OK! *)
*)
  fun f (Struct2.C _) = ()           (* Problem! *)
end;
