(* rs6000gen.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

structure RS6000MC = FLINTComp (
    structure RS6000Coder = Coder (structure M=RS6000Depend
				         and E=RS6000MCodeEmitter)
    structure Gen = CPSgen(structure M = RS6000CM (structure C=RS6000Coder)
			   structure MachSpec = RS6000Spec)
    fun collect() = (RS6000Coder.finish(); KeepRS6000MCode.getCodeString())
)

(*
 * $Log$
 *)
