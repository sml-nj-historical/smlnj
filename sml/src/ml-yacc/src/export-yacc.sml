(* export-yacc.sml
 *
 * ML-Yacc Parser Generator (c) 1991 Andrew W. Appel, David R. Tarditi
 *
 * $Log$
 * Revision 1.2  2000/03/07 04:01:05  blume
 * - size info in BOOTLIST
 *      * no fixed upper limits for number of bootfiles or length of
 *        bootfile names in runtime
 *      * falling back to old behavior if no BOOTLIST size info found
 * - allocation size heuristics in .run-sml
 *      * tries to read cache size from /proc/cpuinfo (this is important for
 *         small-cache Celeron systems!)
 * - install.sh robustified
 * - CM manual updates
 * - paranoid mode
 *      * no more CMB.deliver() (i.e., all done by CMB.make())
 *      * can re-use existing sml.boot.* files
 *      * init.cmi now treated as library
 *      * library stamps for consistency checks
 * - sml.boot.<arch>-<os>/PIDMAP file
 *      * This file is read by the CM startup code.  This is used to minimize
 *        the amount of dynamic state that needs to be stowed away for the
 *        purpose of sharing between interactive system and user code.
 * - CM.Anchor.anchor instead of CM.Anchor.{set,cancel}
 *      * Upon request by Elsa.  Anchors now controlled by get-set-pair
 *        like most other CM state variables.
 * - Compiler.CMSA eliminated
 *      * No longer supported by CM anyway.
 * - fixed bugs in pickler that kept biting Stefan
 *      * past refs to past refs (was caused by the possibility that
 *        ad-hoc sharing is more discriminating than hash-cons sharing)
 *      * integer overflow on LargeInt.minInt
 * - ml-{lex,yacc} build scripts now use new mechanism
 *   for building standalone programs
 * - fixed several gcc -Wall warnings that were caused by missing header
 *   files, missing initializations, etc., in runtime (not all warnings
 *   eliminated, though)
 *
 * Revision 1.1.1.10.4.1  2000/02/20 14:44:34  blume
 * CMB.deliver merged with CMB.make; runtime boot code made more flexible
 *
 * Revision 1.1.1.10  1999/04/17 18:56:11  monnier
 * version 110.16
 *
 * Revision 1.1.1.1  1998/04/08 18:40:16  george
 * Version 110.5
 *
 * Revision 1.2  1997/03/03 17:10:37  george
 * moved callcc related functions to SMLofNJ.Cont
 *
# Revision 1.1.1.1  1997/01/14  01:38:05  george
#   Version 109.24
#
 * Revision 1.3  1996/02/26  16:55:22  jhr
 * Moved exportFn/exportML to SMLofNJ structure.
 *
 * Revision 1.2  1996/02/26  15:02:32  george
 *    print no longer overloaded.
 *    use of makestring has been removed and replaced with Int.toString ..
 *    use of IO replaced with TextIO
 *
 * Revision 1.1.1.1  1996/01/31  16:01:45  george
 * Version 109
 * 
 *)

structure ExportParseGen : sig
    val parseGen : (string * string list) -> OS.Process.status
end = struct
    fun err msg = TextIO.output (TextIO.stdErr, msg)

    exception Interrupt;

    (* This function applies operation to ().  If it handles an interrupt
       signal (Control-C), it raises the exception Interrupt. Example:
       (handleInterrupt foo) handle Interrupt => print "Bang!\n" *)

    fun handleInterrupt (operation : unit -> unit) =
      let exception Done
          val old'handler = Signals.inqHandler(Signals.sigINT)
          fun reset'handler () =
            Signals.setHandler(Signals.sigINT, old'handler)
      in (SMLofNJ.Cont.callcc (fn k =>
             (Signals.setHandler(Signals.sigINT, Signals.HANDLER(fn _ => k));
               operation ();
               raise Done));
           err ("\n--- Interrupt ml-yacc ---\n");
           raise Interrupt)
          handle Done => (reset'handler ())
               | exn  => (reset'handler (); raise exn)
      end

    val exit = OS.Process.exit

    fun parseGen (_, argv) = let
	fun parse_gen () =
	    case argv of
		[file] => (ParseGen.parseGen file; exit OS.Process.success)
	      | _ => (err("Usage: ml-yacc filename\n");
		      exit OS.Process.failure)
    in
	(handleInterrupt parse_gen; OS.Process.success)
	handle Interrupt => OS.Process.failure
	     | ex => (err (String.concat ["? ml-yacc: uncaught exception ",
					  exnMessage ex, "\n"]);
		      OS.Process.failure)
    end
end
