(* bug1222 *)

val x = ref 0;

fun f n : unit = 
     (x := !x + 1;
      f (n+1));

f (valOf Int.maxInt) handle _ => ();
x;  (* should be 1 *)
