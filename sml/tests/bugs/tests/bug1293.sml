(* bug1293 *)

CharVector.mapi (fn (i, c) => (print (Int.toString i ^ "\n"); c))
("ABCDEFG", 2, SOME 3);

