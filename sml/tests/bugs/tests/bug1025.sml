(* bug1025.sml *)

val a = Array.array(1,0w0:Word32.word);
Array.update(a,0,0w1);
Array.sub(a,0);
Array.sub(a,0);
Array.sub(a,0);
Array.sub(a,0);
Array.sub(a,0);
