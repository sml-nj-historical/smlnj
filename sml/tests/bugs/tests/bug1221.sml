(* bug1221.sml *)

structure X = struct val ten = 0w10 val y = Word32.div(ten,ten) end;
