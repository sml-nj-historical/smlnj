(* test52.sml *)
(* keywords: unbound, signature *)

(* testing for secondary compiler bug after unbound signature *)

structure A : FOO = struct end;
