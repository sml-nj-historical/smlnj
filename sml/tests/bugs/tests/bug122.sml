(* bug122.sml *)

structure Y = struct local val x=1 in structure X = struct val y = 1 end end end;

structure Y = let val x=1 in struct structure X = struct val y = 1 end end end;
