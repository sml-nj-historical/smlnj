(* bug276.sml *)

signature Foo1 =
sig
  val foo: string
end;

signature Foo2 =
sig
  include Foo1
  val foo: bool
end;

structure Foo: Foo2 =
struct
  val foo = true
end;
