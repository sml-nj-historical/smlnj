(* bug1219.sml *)

datatype id = ID of Int32.int;
ID (Int32.fromInt 0);

datatype id = ID of Int32.int;
ID (0 : Int32.int);

datatype id = ID of Word32.word;
ID (Word32.fromInt 0);
