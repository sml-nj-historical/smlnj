(* bug591.sml *)
(* uncaught exception Match during elaboration/evaluation of fn expressions *)

fn (th :: _) => [th] | nil => nil;

fn (SOME x) => [x] | NONE => [];
