signature PPC_PSEUDO_INSTR = sig
  structure I : PPCINSTR

  val cvti2d : {reg:int, fd:int} -> I.instruction list 
   (* cvti2d(reg) -- convert integer held reg to 64 bit float held in fd *)
end