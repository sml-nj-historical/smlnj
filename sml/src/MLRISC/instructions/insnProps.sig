(*
 * Basic Instruction properties that must be supported on all architectures.
 *
 * -- Allen
 *)
signature INSN_PROPERTIES = 
sig
   structure C : CELLS
   structure I : INSTRUCTIONS

   sharing I.C = C 

      (* classify instructions *)
   datatype kind = IK_JUMP   (* branches, including returns *)
                 | IK_NOP    (* no ops *)
                 | IK_INSTR  (* normal instructions *)
                 | IK_COPY   (* parallel copy *)
                 | IK_CALL   (* call instructions *)
                 | IK_PHI    (* A phi node (SSA) *)
                 | IK_SINK   (* A sink node (SSA) *)
                 | IK_SOURCE (* A source node (SSA) *)

   val instrKind  : I.instruction -> kind

      (* parallel moves *) 
   val moveInstr  : I.instruction -> bool
   val moveTmpR   : I.instruction -> C.cell option
   val moveDstSrc : I.instruction -> C.cell list * C.cell list

      (* no op *)
   val nop 	  : unit -> I.instruction

      (* jump instruction *)
   val jump       : Label.label -> I.instruction

      (* load immediate; must be within immedRange *)
   val immedRange  : {lo:int, hi:int}
   val loadImmed   : {immed:int, t:C.cell} -> I.instruction
   val loadOperand : {opn:I.operand, t:C.cell} -> I.instruction

     (* 
      * Targets of a branch instruction 
      * precondition: instruction must be of type IK_JUMP.
      *)
   datatype target = LABELLED of Label.label | FALLTHROUGH | ESCAPES
   val branchTargets : I.instruction -> target list

      (* Set the branch target; no effect if not a branch instruction *)
   val setTargets : I.instruction * Label.label list -> I.instruction
 
      (* equality and hashing on operands *)
   val eqOpn      : I.operand * I.operand -> bool
   val hashOpn    : I.operand -> word

      (* Negate the branching condition; raises NegateConditional
       * if it is impossible to negate
       *)
   exception NegateConditional
   val negateConditional : I.instruction -> I.instruction

     (* definition/use for the RA *)
   val defUse     : C.cellkind -> 
                      I.instruction -> (C.cell list * C.cell list)

     (* annotations *)
   val getAnnotations : I.instruction ->
                           I.instruction * Annotations.annotation list
   val annotate       : I.instruction * Annotations.annotation -> I.instruction

   val replicate : I.instruction -> I.instruction
end

