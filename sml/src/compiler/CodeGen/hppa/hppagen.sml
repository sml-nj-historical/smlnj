(* hppagen.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

structure HppaMC = 
  FLINTComp(
    structure HppaGen = HppaCG(structure Emitter=HppaMCEmitter)
    structure Gen=HppaGen.MLTreeGen
    fun collect() = (HppaGen.finish(); CodeString.getCodeString()))



(*
 * $Log: hppagen.sml,v $
 * Revision 1.1.1.1  1997/01/14 01:38:38  george
 *   Version 109.24
 *
 *)
