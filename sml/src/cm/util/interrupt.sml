(*
 * Turning SMLofNJ signals into exceptions
 *
 *   Copyright (c) 1998 by Lucent Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)

signature INTERRUPT = sig

    exception Interrupt

    (*
     * guarded: thunk -> 'a
     *  - run thunk () and return the result
     *  - if the thunk gets interrupted then raise Interrupt
     *)
    val guarded: (unit -> 'a) -> 'a
end

structure Interrupt :> INTERRUPT = struct

    exception Interrupt

    structure Sig = Signals

    val sigINT = Sig.sigINT
    val inqHandler = Sig.inqHandler
    val setHandler = Sig.setHandler

    fun guarded thunk = let
	val oh = inqHandler sigINT
	fun reset () = ignore (setHandler (sigINT, oh))
	fun thunk' () = thunk () handle exn => (reset (); raise exn)
	val callcc = SMLofNJ.Cont.callcc
	val throw = SMLofNJ.Cont.throw
    in
	callcc (fn exitK =>
		(callcc (fn intK =>
			 (setHandler (sigINT, Sig.HANDLER (fn _ => intK));
			  throw exitK (thunk' () before reset ())));
		 reset ();
		 raise Interrupt))
    end
end
