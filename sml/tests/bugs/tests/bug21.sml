(* bug21.sml *)

signature SIG = sig
   exception Foo of int
   val A: int
   val B: int
   val C: int
end;

structure S: SIG =
struct
  exception Foo: int
  val A = 1
  val B = 2
  val C = 3
end;
