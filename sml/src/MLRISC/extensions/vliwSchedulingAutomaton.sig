signature VLIW_SCHEDULING_AUTOMATON =
sig
   structure I : VLIW_INSTRUCTIONS
   structure FU : FUNITS
      sharing FU = I.FU

   type state
   type instrClass 

   exception Hazard

   val startState : state
   val go : state * instrClass -> state
   val instrToClass : I.instruction -> instrClass
   val alternatives : instrClass -> FU.fu list
   val mayConflict  : instrClass * instrClass -> bool
end

(*
 * $Log: vliwSchedulingAutomaton.sig,v $
 * Revision 1.1.1.1  1998/11/16 21:48:27  george
 *   Version 110.10
 *
 *)
