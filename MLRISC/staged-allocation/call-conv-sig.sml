(* call-conv-sig.sml
 *
 * 
 *)

signature CALL_CONV = sig

    structure StagedAllocation : STAGED_ALLOCATION
    structure TargetLang : TARGET_LANG

    type slot = StagedAllocation.slot
    type location_info = StagedAllocation.location_info
    type automaton = {s0 : StagedAllocation.str, step : StagedAllocation.stepper_fn}

    val genAutomaton : unit ->
	  {processParams : automaton, processReturn : automaton}
 
end (* CALL_CONV *)
