(* main.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * $Log$
 * Revision 1.1.1.5  1999/04/17 16:22:23  monnier
 * version 110.11
 *
 * Revision 1.1.1.1  1998/04/08 18:40:05  george
 * Version 110.5
 *
 * Revision 1.1.1.1  1997/01/14 01:37:59  george
 *   Version 109.24
 *
 * Revision 1.1.1.2  1997/01/11  18:52:31  george
 *   ml-burg Version 109.24
 *
 * Revision 1.3  1996/02/26  16:55:12  jhr
 * Moved exportFn/exportML to SMLofNJ structure.
 *
 * Revision 1.2  1996/02/26  15:02:06  george
 *    print no longer overloaded.
 *    use of makestring has been removed and replaced with Int.toString ..
 *    use of IO replaced with TextIO
 *
 * Revision 1.1.1.1  1996/01/31  16:01:25  george
 * Version 109
 * 
 *)

structure Main =
  struct

    fun main (cmdName, argv) = let
	  fun help () = (
		TextIO.output (TextIO.stdErr, "usage: mlburg [<filename>.burg]\n");
		OS.Process.failure)
          in
	    case argv
	     of [] => (
		  BurgEmit.emit (TextIO.stdIn, (fn () => TextIO.stdOut));
		  OS.Process.success)
	      | ("-h"::_) => help ()
	      | ("-help"::_) => help ()
	      | files => let
		  fun findname file = let
		        val {base, ext} = OS.Path.splitBaseExt file
		        in
		          case ext
		           of (SOME("brg" | "burg")) =>
			        OS.Path.joinBaseExt{base=base, ext=SOME "sml"}
		            | _ => file ^ ".sml"
		          (* end case *)
		        end
		  val names = map (fn n => (n,findname n)) files
		  fun emit (inname, outname) = (let
			val s_in = TextIO.openIn inname
			in
			  BurgEmit.emit (s_in, (fn () => (TextIO.openOut outname)))
			end) 
			  handle err => (TextIO.output (TextIO.stdErr, exnMessage err^"\n");
					 raise err)
		  in
		    app emit names;
		    OS.Process.success
		  end
	  end

  (*
   * This is the function to call in an interactive session.
   * Takes a filename (something.burg) as argument, and produces something.sml
   *)
    fun doit s = main ("", [s])

  (* create a stand-alone executable *)
    fun export name = SMLofNJ.exportFn (name, main)

  end;

