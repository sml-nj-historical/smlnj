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
	local
	    fun mode0 (_ : bool option) : bool =
		raise Fail "no btrace module hooked in"
	    val hook = ref { reset = fn () => (), mode = mode0 }
	in
	    fun install { corefns, reset, mode } =
		(hook := { reset = reset, mode = mode };
		 Core.tdp_install corefns)
	    fun reset () = #reset (!hook) ()
	    fun mode x = #mode (!hook) x
	end
	fun report () = Core.tdp_report () ()
	fun save () = Core.tdp_save () ()
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
	    val idk_entry_point = Core.tdp_idk_entry_point
	    val idk_tail_call = Core.tdp_idk_tail_call
	    val idk_non_tail_call = Core.tdp_idk_non_tail_call
	end
    end

  end;
