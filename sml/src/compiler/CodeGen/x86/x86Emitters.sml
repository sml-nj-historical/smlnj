
structure X86MemRegs = X86MemRegs(X86Instr)

(* Assembly code emmitter *)
structure X86AsmEmitter=
  X86AsmEmitter(structure Instr=X86Instr
		structure PseudoOps=X86PseudoOps
		structure Shuffle=X86Shuffle
                structure Stream=X86Stream
		structure MemRegs=X86MemRegs
		val memRegBase=SOME(X86Instr.C.esp))
(* Machine code emitter *)
structure X86MCEmitter = 
  X86MCEmitter(structure Instr=X86Instr
	       structure Shuffle=X86Shuffle
	       structure AsmEmitter=X86AsmEmitter
	       structure MemRegs=X86MemRegs
	       val memRegBase=SOME(X86Instr.C.esp))


structure X86PrintCluster = 
  PrintCluster(structure Flowgraph=X86FlowGraph
	       structure Asm=X86AsmEmitter)

