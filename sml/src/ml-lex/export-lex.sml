(* export-lex.sml
 *
 * $Log$
 * Revision 1.1.1.4  1998/06/05 19:39:48  monnier
 * 110.7
 *
 * Revision 1.2  1997/03/03 17:10:35  george
 * moved callcc related functions to SMLofNJ.Cont
 *
# Revision 1.1.1.1  1997/01/14  01:38:01  george
#   Version 109.24
#
 * Revision 1.3  1996/02/26  16:55:18  jhr
 * Moved exportFn/exportML to SMLofNJ structure.
 *
 * Revision 1.2  1996/02/26  15:02:26  george
 *    print no longer overloaded.
 *    use of makestring has been removed and replaced with Int.toString ..
 *    use of IO replaced with TextIO
 *
 * Revision 1.1.1.1  1996/01/31  16:01:15  george
 * Version 109
 * 
 *)

structure ExportLexGen : sig

    val lexGen : (string * string list) -> OS.Process.status

  end = struct

    exception Interrupt

  (* This function applies operation to ().  If it handles an interrupt
   * signal (Control-C), it raises the exception Interrupt.  Example:
   * (handleInterrupt foo) handle Interrupt => print "Bang!\n"
   *)
    fun handleInterrupt (operation : unit -> unit) =
      let exception Done
          val old'handler = Signals.inqHandler(Signals.sigINT)
          fun reset'handler () =
            Signals.setHandler(Signals.sigINT, old'handler)
      in (SMLofNJ.Cont.callcc (fn k =>
             (Signals.setHandler(Signals.sigINT, Signals.HANDLER(fn _ => k));
               operation ();
               raise Done));
           raise Interrupt)
          handle Done => (reset'handler ())
               | exn  => (reset'handler (); raise exn)
      end

    fun err msg = TextIO.output(TextIO.stdErr, String.concat msg)

    fun lexGen (name, args) = let
	  fun lex_gen () = (case args
		 of [] => (
		      err [name, ": missing filename\n"];
		      OS.Process.exit OS.Process.failure)
		  | files => List.app LexGen.lexGen files
		(* end case *))
	  in
            (handleInterrupt lex_gen; OS.Process.success)
              handle Interrupt => (
			err [name, ": Interrupt\n"];
			OS.Process.failure)
                   | any => (
			err [
			    name, ": uncaught exception ", exnMessage any, "\n"
			  ];
			OS.Process.failure)
          end

  end;
   
fun export name = SMLofNJ.exportFn (name, ExportLexGen.lexGen);

