(* export-lex.sml
 *
 * $Log$
 * Revision 1.4  2000/06/06 02:14:54  blume
 * merging changes from devel branch; new boot files
 *
 * Revision 1.2.2.1  2000/06/02 08:11:06  blume
 * added several appendices to CM manual;
 * merged recent changes to main trunk into devel branch
 *
 * Revision 1.3  2000/06/01 18:33:42  monnier
 * bring revisions from the vendor branch to the trunk
 *
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
 * Revision 1.1.1.9.4.1  2000/02/20 14:44:33  blume
 * CMB.deliver merged with CMB.make; runtime boot code made more flexible
 *
 * Revision 1.1.1.9  1999/12/07 15:40:25  monnier
 * version 110.25
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
	fun lex_gen () =
	    case args of
		[] => (err [name, ": missing filename\n"];
		       OS.Process.exit OS.Process.failure)
	      | files => List.app LexGen.lexGen files
    in
	(handleInterrupt lex_gen; OS.Process.success)
	handle Interrupt => (err [name, ": Interrupt\n"]; OS.Process.failure)
	     | any => (err [name, ": uncaught exception ",
			    exnMessage any, "\n"];
		       OS.Process.failure)
    end
end
