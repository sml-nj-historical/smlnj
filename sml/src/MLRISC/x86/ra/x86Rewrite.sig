signature X86REWRITE = sig
  structure I  : X86INSTR
  val rewriteUse : I.instruction * I.C.cell * I.C.cell -> I.instruction
  val rewriteDef : I.instruction * I.C.cell * I.C.cell -> I.instruction
  val frewriteUse : I.instruction * I.C.cell * I.C.cell -> I.instruction
  val frewriteDef : I.instruction * I.C.cell * I.C.cell -> I.instruction
end

