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
 * $Log: sparcgen.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:47  george
 * Version 110.5
 *
 *)
