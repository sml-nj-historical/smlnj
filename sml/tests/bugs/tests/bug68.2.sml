(* bug68.2.sml *)
(* spurious error message -- ...doesn't match signature spec *)

structure S: sig val x: int end = struct val x = hd "s" end;
