(* alphaPseudoInstr.sig --- alpha pseudo instructions *)

signature ALPHA_PSEUDO_INSTR = 
sig
   structure I : ALPHAINSTR
   structure T : MLTREE
   structure C : ALPHACELLS
     sharing C = I.C
  
   type reduceOpnd = I.operand -> C.cell

   val divlv : {ra:C.cell, rb:I.operand, rc:C.cell} * reduceOpnd -> I.instruction list
   val divl  : {ra:C.cell, rb:I.operand, rc:C.cell} * reduceOpnd -> I.instruction list
   val divlu : {ra:C.cell, rb:I.operand, rc:C.cell} * reduceOpnd -> I.instruction list
   val remlv : {ra:C.cell, rb:I.operand, rc:C.cell} * reduceOpnd -> I.instruction list
   val reml  : {ra:C.cell, rb:I.operand, rc:C.cell} * reduceOpnd -> I.instruction list
   val remlu : {ra:C.cell, rb:I.operand, rc:C.cell} * reduceOpnd -> I.instruction list
   val divqv : {ra:C.cell, rb:I.operand, rc:C.cell} * reduceOpnd -> I.instruction list
   val divq  : {ra:C.cell, rb:I.operand, rc:C.cell} * reduceOpnd -> I.instruction list
   val divqu : {ra:C.cell, rb:I.operand, rc:C.cell} * reduceOpnd -> I.instruction list
   val remqv : {ra:C.cell, rb:I.operand, rc:C.cell} * reduceOpnd -> I.instruction list
   val remq  : {ra:C.cell, rb:I.operand, rc:C.cell} * reduceOpnd -> I.instruction list
   val remqu : {ra:C.cell, rb:I.operand, rc:C.cell} * reduceOpnd -> I.instruction list

   val cvtls : {opnd:I.operand, fd:C.cell} * reduceOpnd -> I.instruction list
   val cvtlt : {opnd:I.operand, fd:C.cell} * reduceOpnd -> I.instruction list
   val cvtqs : {opnd:I.operand, fd:C.cell} * reduceOpnd -> I.instruction list
   val cvtqt : {opnd:I.operand, fd:C.cell} * reduceOpnd -> I.instruction list

   val cvtsl : {mode:T.rounding_mode, fs:C.cell, rd:C.cell} -> I.instruction list
   val cvttl : {mode:T.rounding_mode, fs:C.cell, rd:C.cell} -> I.instruction list
   val cvtsq : {mode:T.rounding_mode, fs:C.cell, rd:C.cell} -> I.instruction list
   val cvttq : {mode:T.rounding_mode, fs:C.cell, rd:C.cell} -> I.instruction list
end 

