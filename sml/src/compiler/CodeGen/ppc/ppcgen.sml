(* ppcgen.sml
 *
 * COPYRIGHT (c) 1999 Bell Laboratories.
 *
 *)

structure PPCMC = 
  FLINTComp(
    structure Gen=PPCCG
    fun collect() = (PPCCG.finish(); CodeString.getCodeString()))
