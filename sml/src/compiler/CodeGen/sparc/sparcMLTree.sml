(* sparcMLTree.sml --- customize MLRISC for the Sparc.
 * 
 * COPYRIGHT (c) 1998 AT&T Bell Laboratories.
 *)

structure SparcPseudoOps = PseudoOpsBig(SparcSpec)

structure SparcStream = InstructionStream(SparcPseudoOps)

structure SparcMLTree = 
  MLTreeF(structure Constant=SMLNJConstant
	  structure Region=CPSRegions
	  structure Stream=SparcStream
	  structure Extension=Sparc_SMLNJMLTreeExt
         )

(* specialised sparc instruction set *)
structure SparcInstr = 
  SparcInstr(
    LabelExp
       (structure T = SparcMLTree
	val labelFmt = {gPrefix="L", aPrefix="L"}
        fun h _ _ = 0w0 fun eq _ _ = false
        val hashRext = h and hashFext = h and hashCCext = h and hashSext = h
        val eqRext = eq and eqFext = eq and eqCCext = eq and eqSext = eq
        ))

structure SparcProps = SparcProps(SparcInstr)

structure SparcPseudoInstrs = SparcPseudoInstrs(SparcInstr)

structure SparcShuffle = SparcShuffle(SparcInstr)

structure SparcAsmEmitter = 
  SparcAsmEmitter(structure Instr=SparcInstr
		  structure Shuffle=SparcShuffle
                  structure Stream = SparcStream
		  structure PseudoOps=SparcPseudoOps
                  val V9 = false)

structure SparcMCEmitter = 
  SparcMCEmitter(structure Instr=SparcInstr
		 structure Assembler=SparcAsmEmitter
                 structure Stream = SparcStream
		 structure CodeString=CodeString)

(* flowgraph data structure specialized to Sparc instructions *)
structure SparcCFG = 
  ControlFlowGraph
     (structure I = SparcInstr
      structure PseudoOps = SparcPseudoOps
      structure GraphImpl = DirectedGraph
      structure InsnProps = SparcProps
      structure Asm = SparcAsmEmitter)


