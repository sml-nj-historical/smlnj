(* internals.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * This structure (SMLofNJ.Internals) is a gathering place for internal
 * features that need to be exposed outside the boot directory.
 *)

structure Internals : INTERNALS = struct

    structure CleanUp = CleanUp
    structure ProfControl = ProfControl
    structure GC = GC

    val prHook = PrintHook.prHook

    val initSigTbl = InternalSignals.initSigTbl
    val clearSigTbl = InternalSignals.clearSigTbl
    val resetSigTbl = InternalSignals.resetSigTbl

    val resetTimers = InternalTimer.resetTimers

    structure TDP = struct
        type plugin = Core.tdp_plugin

	val active_plugins = Core.tdp_active_plugins

	fun reserve n = Core.tdp_reserve n
	fun reset () = Core.tdp_reset ()

	val idk_entry_point = Core.tdp_idk_entry_point
	val idk_tail_call = Core.tdp_idk_tail_call
	val idk_non_tail_call = Core.tdp_idk_non_tail_call

	val mode = ref false
    end

    structure BTrace = struct
	local
	    val te_hook = ref (fn () => Fail "bogus backtrace exception")
	in
	    fun install { plugin, mktriggerexn } =
		(te_hook := mktriggerexn;
		 TDP.active_plugins := plugin :: !TDP.active_plugins)
	    fun trigger () = raise (!te_hook())
	end
    end

end
