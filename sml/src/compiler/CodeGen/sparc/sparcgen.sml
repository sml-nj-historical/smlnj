(* sparcgen.sml
 *
 * COPYRIGHT (c) 1998 AT&T Bell Laboratories.
 *)

structure SparcMC = 
  FLINTComp(
    structure SparcGen = SparcCG(structure Emitter=SparcMCEmitter)
    structure Gen=SparcGen.MLTreeGen
    fun collect() = (SparcGen.finish(); CodeString.getCodeString()))


(*
 * $Log: sparcgen.sml,v $
 * Revision 1.1.1.1  1998/08/05 19:37:50  george
 *   Release 110.7.4
 *
 *)
