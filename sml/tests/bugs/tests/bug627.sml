(* bug627.sml *)
(* blast-writing objects outside the heap leads to failure *)

structure Blast =
struct
  val s : string = Compiler.banner
  structure B = BinIO
  fun outblast () = let
	val s = B.openOut "/tmp/testblast"
	in
	  B.output(s, Unsafe.blastWrite s); B.closeOut s
	end
  fun inblast () = let
	val s = B.openIn "/tmp/testblast"
	val res : string = Unsafe.blastRead(B.inputAll s)
	in
	  B.closeIn s;
	  print "Got back: "; print res; print " : from testblast\n"
	end
  val _ = outblast ()
  val _ = inblast ()
end;
