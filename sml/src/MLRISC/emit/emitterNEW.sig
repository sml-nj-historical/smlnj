(* emitterNEW.sig
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

(** emitter - emit assembly or machine code **)

(* Note:
 *	assembly code: Each of the emit functions outputs the 
 * appropriate assembly instructions to a file. The stream to
 * this file can be hardwired.
 *
 *      machine code: Each of the emit functions outputs the 
 * appropriate binary output to a bytearray created in a special
 * structure reserved for this purpose.
 *
 *)
signature EMITTER_NEW = sig
  structure I : INSTRUCTIONS
  structure P : PSEUDO_OPS

  val defineLabel  : Label.label -> unit
  val emitInstr : I.instruction * int Intmap.intmap -> unit
  val comment : string -> unit
  val pseudoOp : P.pseudo_op -> unit
  val init : int -> unit
end  




(*
 * $Log: emitterNEW.sig,v $
 * Revision 1.1.1.1  1998/11/16 21:48:19  george
 *  Version 110.10
 *
 * Revision 1.2  1998/10/06 14:07:46  george
 * Flowgraph has been removed from modules that do not need it.
 * Changes to compiler/CodeGen/*/*{MLTree,CG}.sml necessary.
 * 						[leunga]
 *
 * Revision 1.1.1.1  1998/04/08 18:39:02  george
 * Version 110.5
 *
 *)
