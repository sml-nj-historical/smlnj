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
        exception BTrace of unit -> string list
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
	fun trigger () = raise BTrace (report ())
	fun save () = Core.bt_save () ()
    end

  end;
