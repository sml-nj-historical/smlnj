(* bug1177.sml *)

val z =
    let val tbl = ref nil
	fun ins x = tbl := x :: !tbl
     in ins 1
    end;

