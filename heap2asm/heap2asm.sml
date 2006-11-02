(* heap2asm.sml
 *
 *   Generating an assembly code file corresponding to a heap image.
 *
 * Copyright (c) 2005 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure Main: sig
    val main: string * string list -> OS.Process.status
end = struct

    val N = 20

    fun one (inf, outf) =
	let val (si, so) = (TextIO.openIn inf, TextIO.openOut outf)
	    fun out s = TextIO.output (so, s)
	    fun finish n =
		(out ".text\n\t.align 2\n_smlnj_heap_image_len:\n\t.long ";
		 out (Int.toString n); out "\n")
	    fun line l =
		let val bl = map (Int.toString o ord) (String.explode l)
		in out ("\t.byte " ^ String.concatWith "," bl ^ "\n")
		end
	    fun lines n =
		case TextIO.inputN (si, N) of
		    "" => finish n
		  | l => let val s = size l
			 in line l; if s < N then finish (n+s) else lines (n+s)
			 end
	in out "\t.globl _smlnj_heap_image\n\
	       \\t.globl _smlnj_heap_image_len\n\
	       \.text\n\t.align 2\n\
	       \_smlnj_heap_image:\n";
	   lines 0;
	   TextIO.closeIn si; TextIO.closeOut so
	end

    fun complain (p, s) =
	(TextIO.output (TextIO.stdErr, concat [p, ": ", s, "\n"]);
	 OS.Process.failure)

    fun main (p, [inf, outf]) =
	((one (inf, outf); OS.Process.success)
	 handle e => complain (p, "exception: " ^ General.exnMessage e))
      | main (p, _) = complain (p, "usage: " ^ p ^ " heapfile asmfile")
end
