(* sparcgen.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

structure SparcMC = FLINTComp (
    structure SparcC = Coder(structure M = SparcInstr and E = SparcMCEmit)
    structure Gen=CPSgen(structure M = SparcCM(structure C = SparcC)
			 structure MachSpec = SparcSpec)
    fun collect() = (SparcC.finish(); SparcMCode.getCodeString())
)


(*
 * $Log$
 *)
