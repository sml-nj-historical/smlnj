(* internals.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * This structure (SMLofNJ.Internals) is a gathering place for internal
 * features that need to be exposed outside the boot directory.
 *)

structure Internals : INTERNALS =
  struct

    structure CleanUp = CleanUp
    structure ProfControl = ProfControl
    structure GC = GC

    val prHook = PrintHook.prHook

    val initSigTbl = InternalSignals.initSigTbl
    val clearSigTbl = InternalSignals.clearSigTbl
    val resetSigTbl = InternalSignals.resetSigTbl

    val resetTimers = InternalTimer.resetTimers

    structure BTrace = struct
        val mode = let
	    val state = ref false
	    fun access NONE = !state
	      | access (SOME change) = !state before state := change
	in
	    access
	end
	local
	    val hook = ref { reset = fn () => () }
	in
	    fun install { corefns, reset } =
		(hook := { reset = reset };
		 Core.bt_install corefns)
	    fun reset () = #reset (!hook) ()
	end
	fun report () = Core.bt_report () ()
	fun save () = Core.bt_save () ()
	local
	    exception BTraceTriggered of unit -> string list
	in
	    (* The following function must be compiled with BT-instrumentation
	     * turned off because it relies on its exception handler to _not_
	     * restore the bt-history! *)
	    fun bthandle { work, hdl } = let
		val restore = save ()
	    in
		work ()
		handle e as BTraceTriggered do_report' =>
		       (restore (); hdl (e, do_report' ()))
		     | e => let
			   val do_report = report ()
		       in
			   restore ();
			   hdl (e, do_report ())
		       end
	    end
	    fun trigger () = raise BTraceTriggered (report ())
	end
    end

  end;
