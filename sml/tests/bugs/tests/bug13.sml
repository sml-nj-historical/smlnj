(* bug13.sml *)

signature FOO = 
sig
   type T1 and T2
   val x1: T1 and x2: T2
   sharing type T1 = T2
end;

structure Foo:> FOO =
struct
   datatype T1 = CON
   type T2 = T1
   val x1 = CON and x2 = CON
end;

[Foo.x1, Foo.x2];
