(* bug411.sml *)
(* raised Runbind exception evaluating x *)

structure A = struct val x = 1 end;

structure B = struct structure A = A; val y = 2 end;

open A;
open B;

x;
