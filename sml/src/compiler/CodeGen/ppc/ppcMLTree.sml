(* COPYRIGHT (c) 1999 Lucent Technologies, Bell Labs. *)

structure PPCPseudoOps = PseudoOpsBig(PPCSpec)

structure PPCStream = InstructionStream(PPCPseudoOps)

structure PPCMLTree = 
  MLTreeF(structure Constant=SMLNJConstant
	  structure Region=CPSRegions
	  structure Stream=PPCStream
	  structure Extension=SMLNJMLTreeExt
         )

(* specialised powerpc instruction set *)
structure PPCInstr = 
  PPCInstr(
    LabelExp
       (structure T = PPCMLTree
        fun h _ _ = 0w0 fun eq _ _ = false
        val hashRext = h and hashFext = h and hashCCext = h and hashSext = h
        val eqRext = eq and eqFext = eq and eqCCext = eq and eqSext = eq
        ))

structure PPCProps = PPCProps(PPCInstr)

structure PPCShuffle = PPCShuffle(PPCInstr)

(* Flowgraph data structure specialized to DEC alpha instructions *)
structure PPCFlowGraph = 
  FlowGraph(structure I=PPCInstr
	    structure P=PPCPseudoOps
           )

structure PPCAsmEmitter=
  PPCAsmEmitter(structure Instr=PPCInstr
		structure PseudoOps=PPCPseudoOps  
                structure Stream=PPCStream
		structure Shuffle = PPCShuffle)

structure PPCMCEmitter = 
  PPCMCEmitter(structure Instr=PPCInstr
	       structure PseudoOps=PPCPseudoOps
               structure Stream=PPCStream
	       structure CodeString=CodeString)



