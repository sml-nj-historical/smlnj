(* ppcgen.sml
 *
 * COPYRIGHT (c) 1999 Bell Laboratories.
 *
 *)

structure PPCMC = 
  FLINTComp(
    structure Gen=PPCCG
    fun collect name = (PPCCG.finish name; CodeString.getCodeString()))
