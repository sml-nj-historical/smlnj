(* bug1413.1.sml *)

val execute = Unix.streamsOf o Unix.execute;
val (instrm,outstrm) = execute("cat",[]);
TextIO.output(outstrm,"abcd");
TextIO.flushOut outstrm;
TextIO.closeOut outstrm;
val s = TextIO.inputAll instrm;
