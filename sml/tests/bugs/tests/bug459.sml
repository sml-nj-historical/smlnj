(* bug459.sml *)

signature foosig = sig val foo: 'a -> int end;
structure foostruct:foosig = struct fun foo x = x end;
