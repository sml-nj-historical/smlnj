(* alpha32gen.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

structure Alpha32MC = 
  FLINTComp(
    structure Alpha32Gen = Alpha32CG(structure Emitter=Alpha32MCEmitter)
    structure Gen=Alpha32Gen.MLTreeGen
    fun collect() = (Alpha32Gen.finish(); CodeString.getCodeString()))



(*
 * $Log: alpha32gen.sml,v $
 * Revision 1.1.1.1  1997/01/14 01:38:08  george
 *   Version 109.24
 *
 *)
