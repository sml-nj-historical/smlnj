(* bug 14  Bad printing of list values *)

    datatype Foo = FOO of int list
    val it = FOO [1, 2, 3]

