(* ppcgen.sml
 *
 * COPYRIGHT (c) 1999 Bell Laboratories.
 *
 *)

structure PPCMC = 
  FLINTComp(
    structure PPCGen = PPCCG(structure Emitter=PPCMCEmitter)
    structure Gen=PPCGen
    fun collect() = (PPCGen.finish(); CodeString.getCodeString()))
