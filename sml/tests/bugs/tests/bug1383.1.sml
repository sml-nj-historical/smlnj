(* bug1383.1.sml *)

Char.toCString(chr 0);

String.toCString("hello\000123there");
