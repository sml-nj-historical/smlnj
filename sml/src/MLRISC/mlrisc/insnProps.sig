(* insnProps.sig --- instruction set properties
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)
signature INSN_PROPERTIES = sig
   structure C : CELLS
   structure I : INSTRUCTIONS

   sharing I.C = C

   datatype kind = IK_JUMP | IK_NOP | IK_INSTR
   datatype target = LABELLED of Label.label | FALLTHROUGH | ESCAPES

   val instrKind     : I.instruction -> kind
      (* kind of instruction  *)

   val moveInstr     : I.instruction -> bool
      (* is the instruction a move? Assumed to have exactly one
       * source and one destination 
       *)

   val moveTmpR : I.instruction -> int option
      (* temporary register associated with parallel move
       * instructions if any.
       *)

   val moveDstSrc : I.instruction -> int list * int list
      (* source and destinations associated with a parallel move *)

   val branchTargets : I.instruction -> target list
      (* targets of an instruction. The instruction kind must be IK_JUMP *)

   val defUseR	     : I.instruction -> int list * int list
      (* general purpose registers def/use *)

   val defUseF	     : I.instruction -> int list * int list
      (* floating point register def/use *)

   val nop 	     : unit -> I.instruction
      (* generate a nop *)
end


(*
 * $Log: insnProps.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:02  george
 * Version 110.5
 *
 *)
