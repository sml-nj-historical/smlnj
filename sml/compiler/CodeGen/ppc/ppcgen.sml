(* ppcgen.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure PPCMC = FLINTComp(
    structure Gen=PPCCG
    fun collect epthunk = (
	  PPCCG.finish ();
	  CodeString.getCodeString (epthunk ())))
