(* hppagen.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

structure HppaMC =
  CPSCompFn(
    structure Gen = HppaCG
    fun collect epthunk = (HppaCG.finish ();
			   CodeString.getCodeString (epthunk ())))


