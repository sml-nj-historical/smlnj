(* bug1526.1.sml *)

(* all three tests should result in SOME(#"\""). they do not *)
val test1 = Char.fromString "\"";
val test2 = SOME(String.sub ("\"", 0));
val test3 = Char.fromString (Char.toString #"\"");
