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
        fun h _ _ = 0w0 fun eq _ _ = false
        val hashRext = h and hashFext = h and hashCCext = h and hashSext = h
        val eqRext = eq and eqFext = eq and eqCCext = eq and eqSext = eq
        ))

structure X86Props = X86Props(X86Instr)

structure X86Rewrite = X86Rewrite(X86Instr)

structure X86Shuffle = X86Shuffle(X86Instr)


(* Flowgraph data structure specialized to X86 instructions *)
structure X86FlowGraph = 
  FlowGraph(structure I=X86Instr 
	    structure P=X86PseudoOps
           )



