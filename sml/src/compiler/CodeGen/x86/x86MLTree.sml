(* specialised X86 instruction set *)
structure X86Instr = 
  X86Instr(structure LabelExp=SMLNJLabelExp
	   structure Region=CPSRegions)

structure X86Props = X86Props(X86Instr)

structure X86Rewrite = X86Rewrite(X86Instr)

structure X86Shuffle = X86Shuffle(X86Instr)

structure X86PseudoOps = 
  PseudoOpsLittle(structure M=X86Spec val nop = SOME(0wx90:Word8.word))

structure X86MemRegs = X86MemRegs(X86Instr)

(* Flowgraph data structure specialized to X86 instructions *)
structure X86FlowGraph = 
  FlowGraph(structure I=X86Instr 
	    structure P=X86PseudoOps
           )

structure X86Stream = InstructionStream(X86PseudoOps)


(* Assembly code emmitter *)
structure X86AsmEmitter=
  X86AsmEmitter(structure Instr=X86Instr
		structure PseudoOps=X86PseudoOps
		structure Shuffle=X86Shuffle
                structure Stream=X86Stream
		structure MemRegs=X86MemRegs)
  
(* Machine code emitter *)
structure X86MCEmitter = 
  X86MCEmitter(structure Instr=X86Instr
	       structure Shuffle=X86Shuffle
	       structure AsmEmitter=X86AsmEmitter
	       structure MemRegs=X86MemRegs)

(* MLTree specialization *)
structure X86MLTree = 
  MLTreeF(structure LabelExp=SMLNJLabelExp
	  structure R=CPSRegions
	  structure S=X86Stream
         )
