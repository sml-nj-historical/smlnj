(* bug1599.1.sml *)

signature SIG1 =
sig
  type t
end;

signature SIG2 =
sig
  type t
  val f: t -> unit
end;

signature SIG3 =
sig
  structure S: SIG1
end;

functor MkA(structure S1: SIG3): SIG3 =
struct
  open S1
end;

functor MkB(structure A: SIG2): SIG3 =
  MkA(structure S1 = struct structure S = A end);

functor MkC(structure Ta: SIG2
            structure B: SIG3 where S = Ta) = struct end;

structure A =
struct
  type t = int
  val f = ignore
end;

structure B = MkB(structure A = A);

structure C = MkC(structure A = A
                  structure B = B);
