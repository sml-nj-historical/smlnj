(* 182.sml *)
(* module test: datatype replication declarations *)

structure S =
struct
  datatype t = A | B of t
end;

structure T =
struct
  datatype s = datatype S.t
  fun f(x: s) = case x of A => true | B y => false
end;

val x = T.f(S.A);
