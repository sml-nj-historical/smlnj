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
  structure F : FLOWGRAPH  
    sharing F.I = I

  val defineLabel  : Label.label -> unit
  val emitInstr : I.instruction * int Intmap.intmap -> unit
  val comment : string -> unit
  val pseudoOp : F.P.pseudo_op -> unit
  val init : int -> unit
end  




(*
 * $Log: emitterNEW.sig,v $
 * Revision 1.2  1997/07/17 12:28:55  george
 *   The regmap is now represented as an int map rather than using arrays.
 *
# Revision 1.1.1.1  1997/04/19  18:14:20  george
#   Version 109.27
#
 *)
