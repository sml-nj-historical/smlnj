(* bug1019.sml *)

fun loop (0, ws, rs) = ()
  | loop (n, ws, rs) = app print [Word32.toString ws, Real.toString rs];
