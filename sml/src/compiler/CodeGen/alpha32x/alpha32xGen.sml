(* alpha32gen.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

structure Alpha32XMC = 
  FLINTComp(
    structure Alpha32Gen = Alpha32XCG(structure Emitter=Alpha32XMCEmitter)
    structure Gen=Alpha32Gen.MLTreeGen
    fun collect() = (Alpha32Gen.finish(); CodeString.getCodeString()))



(*
 * $Log: alpha32xGen.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:54  george
 * Version 110.5
 *
 *)
