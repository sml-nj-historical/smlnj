(* bug273.sml *)

val x = fn y => let val f = ref in f end;
