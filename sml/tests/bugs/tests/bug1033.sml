(* bug1033.sml *)

val x = ref (0w0:Word32.word);
SMLofNJ.Internals.GC.doGC 1;
x:=0w1;
SMLofNJ.Internals.GC.doGC 1;
