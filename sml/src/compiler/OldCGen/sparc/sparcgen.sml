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
 * Revision 1.1.1.1  1997/01/14 01:38:45  george
 *   Version 109.24
 *
 *)
