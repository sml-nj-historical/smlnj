structure Alpha32PseudoOps = 
    PseudoOpsLittle(structure M = Alpha32Spec val nop = NONE)

structure Alpha32Stream = InstructionStream(Alpha32PseudoOps)

structure Alpha32MLTree = 
  MLTreeF(structure Constant=SMLNJConstant
	  structure Region=CPSRegions
	  structure Stream=Alpha32Stream
	  structure Extension=SMLNJMLTreeExt
         )

(* specialised alpha32 instruction set *)
structure Alpha32Instr = 
  AlphaInstr(
    LabelExp
       (structure T = Alpha32MLTree
        fun h _ _ = 0w0 fun eq _ _ = false
        val hashRext = h and hashFext = h and hashCCext = h and hashSext = h
        val eqRext = eq and eqFext = eq and eqCCext = eq and eqSext = eq
        ))

structure Alpha32Props = AlphaProps(Alpha32Instr)

structure Alpha32Shuffle = AlphaShuffle(Alpha32Instr)


structure Alpha32AsmEmitter=
  AlphaAsmEmitter(structure Instr=Alpha32Instr
	          structure PseudoOps=Alpha32PseudoOps
                  structure Stream=Alpha32Stream
		  structure Shuffle = Alpha32Shuffle)

structure Alpha32MCEmitter = 
  AlphaMCEmitter(structure Instr=Alpha32Instr
		 structure PseudoOps=Alpha32PseudoOps
                 structure Stream=Alpha32Stream
		 structure CodeString=CodeString)

structure Alpha32PseudoInstrs = Alpha32PseudoInstrs(Alpha32Instr)

(* Flowgraph data structure specialized to DEC alpha instructions *)
structure Alpha32CFG = 
  ControlFlowGraph
     (structure I = Alpha32Instr
      structure PseudoOps = Alpha32PseudoOps
      structure GraphImpl = DirectedGraph
      structure InsnProps = Alpha32Props
      structure Asm = Alpha32AsmEmitter)
