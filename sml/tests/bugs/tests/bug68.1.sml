(* bug68.1.sml *)

structure S: sig val x: int end = struct val x = hd "s" end;
