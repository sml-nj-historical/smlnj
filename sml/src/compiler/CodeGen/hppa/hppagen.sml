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
 * $Log$
 *)
