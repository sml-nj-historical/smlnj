(* bug240.sml *)
(* concrete printing of abstype value *)

abstype Foo = Foo of int
  with fun foo i = Foo i end;
val bar = foo 0;  (* prints "> val bar = Foo 0 : Foo" *)
