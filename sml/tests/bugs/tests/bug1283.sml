(* bug1283.sml *)

type 'a queue = 'a list * 'a list;
val empty : 'a queue = (nil, nil);

