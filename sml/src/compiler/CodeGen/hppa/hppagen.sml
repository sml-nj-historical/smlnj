(* hppagen.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

structure HppaMC = 
  FLINTComp(
    structure HppaGen = HppaCG(structure Emitter=HppaMCEmitter)
    structure Gen=HppaGen
    fun collect() = (HppaGen.finish(); CodeString.getCodeString()))



(*
 * $Log: hppagen.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:55  george
 * Version 110.5
 *
 *)
