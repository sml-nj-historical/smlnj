(* alphaPseudoInstr.sig --- alpha pseudo instructions *)

signature ALPHA_PSEUDO_INSTR = 
sig
   structure I : ALPHAINSTR
   structure T : MLTREE
  
   type reduceOpnd = I.operand -> int

   val divlv : {ra:int, rb:I.operand, rc:int} * reduceOpnd -> I.instruction list
   val divl  : {ra:int, rb:I.operand, rc:int} * reduceOpnd -> I.instruction list
   val divlu : {ra:int, rb:I.operand, rc:int} * reduceOpnd -> I.instruction list
   val remlv : {ra:int, rb:I.operand, rc:int} * reduceOpnd -> I.instruction list
   val reml  : {ra:int, rb:I.operand, rc:int} * reduceOpnd -> I.instruction list
   val remlu : {ra:int, rb:I.operand, rc:int} * reduceOpnd -> I.instruction list
   val divqv : {ra:int, rb:I.operand, rc:int} * reduceOpnd -> I.instruction list
   val divq  : {ra:int, rb:I.operand, rc:int} * reduceOpnd -> I.instruction list
   val divqu : {ra:int, rb:I.operand, rc:int} * reduceOpnd -> I.instruction list
   val remqv : {ra:int, rb:I.operand, rc:int} * reduceOpnd -> I.instruction list
   val remq  : {ra:int, rb:I.operand, rc:int} * reduceOpnd -> I.instruction list
   val remqu : {ra:int, rb:I.operand, rc:int} * reduceOpnd -> I.instruction list

   val cvtls : {opnd:I.operand, fd:int} * reduceOpnd -> I.instruction list
   val cvtlt : {opnd:I.operand, fd:int} * reduceOpnd -> I.instruction list
   val cvtqs : {opnd:I.operand, fd:int} * reduceOpnd -> I.instruction list
   val cvtqt : {opnd:I.operand, fd:int} * reduceOpnd -> I.instruction list

   val cvtsl : {mode:T.rounding_mode, fs:int, rd:int} -> I.instruction list
   val cvttl : {mode:T.rounding_mode, fs:int, rd:int} -> I.instruction list
   val cvtsq : {mode:T.rounding_mode, fs:int, rd:int} -> I.instruction list
   val cvttq : {mode:T.rounding_mode, fs:int, rd:int} -> I.instruction list
end 

