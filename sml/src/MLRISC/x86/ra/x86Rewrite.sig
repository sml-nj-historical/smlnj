signature X86REWRITE = sig
  structure I  : X86INSTR
  val rewriteUse : I.C.regmap * I.instruction * int * int -> I.instruction
  val rewriteDef : I.C.regmap * I.instruction * int * int -> I.instruction
  val frewriteUse : I.C.regmap * I.instruction * int * int -> I.instruction
  val frewriteDef : I.C.regmap * I.instruction * int * int -> I.instruction
end

