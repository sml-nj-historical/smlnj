(* alpha32gen.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

structure Alpha32MC = 
  FLINTComp(
    structure Alpha32Gen = Alpha32CG(structure Emitter=Alpha32MCEmitter)
    structure Gen=Alpha32Gen
    fun collect() = (Alpha32Gen.finish(); CodeString.getCodeString()))



(*
 * $Log: alpha32gen.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:54  george
 * Version 110.5
 *
 *)
