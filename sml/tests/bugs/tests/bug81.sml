(* bug85.sml *)

structure Foo : sig
  type foo
  val f : foo -> int
end =
struct
  type Foo = int
  fun f x = x
end;
