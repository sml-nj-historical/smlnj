(* bug863.sml *)
(* 863. Compiler bug: PPVal.switch: none of the datacons matched *)

structure A = struct val p = 6 end;
structure B = struct val p = true end;
open B A;
