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
 * $Log$
 *)
