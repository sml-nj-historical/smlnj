(* bug74.sml *)
(* withtype with identity type definition (printing only?) *)

datatype Foo = FOO of Forest
withtype Forest = Tree list
     and Tree = Foo;
