(* bug222.sml *)

(* ref type not being recognized as unconditionally an equality type *)

fun silly x = (ref x = ref x);
silly not;

