(* alphaPseudoInstr.sig --- alpha pseudo instructions *)

signature ALPHA_PSEUDO_INSTR = sig
   structure I : ALPHAINSTR
  
   type reduceOpnd = I.operand -> int

   val divl  : {ra:int, rb:I.operand, rc:int} * reduceOpnd -> I.instruction list
   val divlu : {ra:int, rb:I.operand, rc:int} * reduceOpnd -> I.instruction list
   val divq  : {ra:int, rb:I.operand, rc:int} * reduceOpnd -> I.instruction list
   val divqu : {ra:int, rb:I.operand, rc:int} * reduceOpnd -> I.instruction list

   val cvti2s : {opnd:I.operand, fd:int} * reduceOpnd  -> I.instruction list
   val cvti2d : {opnd:I.operand, fd:int} * reduceOpnd  -> I.instruction list
end 

