structure X86PseudoOps = 
  PseudoOpsLittle(structure M=X86Spec val nop = SOME(0wx90:Word8.word))
structure X86Stream = InstructionStream(X86PseudoOps)

(* MLTree specialization *)
structure X86MLTree = 
  MLTreeF(structure Constant = SMLNJConstant
          structure Region=CPSRegions
	  structure Stream=X86Stream
	  structure Extension=X86_SMLNJMLTreeExt)


(* specialised X86 instruction set *)
structure X86Instr = 
  X86Instr(
    LabelExp
       (structure T = X86MLTree
	val labelFmt = {gPrefix="L", aPrefix="L"}
        fun h _ _ = 0w0 fun eq _ _ = false
        val hashRext = h and hashFext = h and hashCCext = h and hashSext = h
        val eqRext = eq and eqFext = eq and eqCCext = eq and eqSext = eq
        ))

structure X86MemRegs = X86MemRegs(X86Instr)
structure X86Props = X86Props(X86Instr)
structure X86Rewrite = X86Rewrite(X86Instr)
structure X86Shuffle = X86Shuffle(X86Instr)

(* Assembly code emmitter *)
structure X86AsmEmitter=
  X86AsmEmitter(structure Instr=X86Instr
		structure Shuffle=X86Shuffle
		structure MemRegs=X86MemRegs
		val memRegBase=SOME(X86Instr.C.esp))


(* Machine code emitter *)
structure X86MCEmitter = 
  X86MCEmitter(structure Instr=X86Instr
	       structure Shuffle=X86Shuffle
	       structure AsmEmitter=X86AsmEmitter
	       structure MemRegs=X86MemRegs
	       val memRegBase=SOME(X86Instr.C.esp))

(* Flowgraph data structure specialized to X86 instructions *)
structure X86CFG = 
  ControlFlowGraph
     (structure I = X86Instr
      structure PseudoOps = X86PseudoOps
      structure GraphImpl = DirectedGraph
      structure InsnProps = X86Props
      structure Asm = X86AsmEmitter)
  
