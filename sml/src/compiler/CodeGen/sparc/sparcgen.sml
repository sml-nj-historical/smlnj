(* sparcgen.sml
 *
 * COPYRIGHT (c) 1998 AT&T Bell Laboratories.
 *)

structure SparcMC = 
  FLINTComp(
    structure Gen=SparcCG
    fun collect() = (SparcCG.finish(); CodeString.getCodeString()))


