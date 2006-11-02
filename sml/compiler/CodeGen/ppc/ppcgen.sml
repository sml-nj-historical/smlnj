(* ppcgen.sml
 *
 * COPYRIGHT (c) 1999 Bell Laboratories.
 *
 *)

structure PPCMC = 
  FLINTComp(
    structure Gen=PPCCG
    fun collect epthunk = (PPCCG.finish ();
			   CodeString.getCodeString (epthunk ())))
