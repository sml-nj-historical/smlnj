(* bug1264.sml *)

signature S =
sig
  type genT
  type T
  val embed : T -> genT
end;

structure S =
struct
  type genT = int
  type T    = int
  val embed = fn x => x
end;

signature B =
sig
  type genT
  functor mkS() : S where type genT = genT
end;

structure B :> B =
struct
  type genT = S.genT
  functor mkS() = S
end;
