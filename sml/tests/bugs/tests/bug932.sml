(* bug932.sml *)
(* Compiler bug: applyTyfun: not enough arguments *)

signature A =
sig
  type 'a f = 'a
end;

structure B: A =
struct
  type 'a f = 'a
end;
