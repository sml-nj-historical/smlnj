(* bug29.sml *)

structure Foo =
struct
  val x = Compiler.Interact.useStream(TextIO.openString "val _ = Foo.x;")
end;
