(* hppaMLTree.sml --- customize MLRISC for the HPPA.
 * 
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

structure HppaPseudoOps = PseudoOpsBig(HppaSpec)

structure HppaStream = InstructionStream(HppaPseudoOps)

structure HppaMLTree = 
  MLTreeF(structure Constant=SMLNJConstant
	  structure Region=CPSRegions
	  structure Stream=HppaStream
	  structure Extension=SMLNJMLTreeExt
         )

(* specialised hppa instruction set *)
structure HppaInstr = 
  HppaInstr(
    LabelExp
       (structure T = HppaMLTree
        fun h _ _ = 0w0 fun eq _ _ = false
        val hashRext = h and hashFext = h and hashCCext = h and hashSext = h
        val eqRext = eq and eqFext = eq and eqCCext = eq and eqSext = eq
        ))

structure HppaShuffle = HppaShuffle(HppaInstr)

structure HppaProps = HppaProps(HppaInstr)

(* flowgraph data structure specialized to Hppa instructions *)
structure HppaFlowGraph = 
  FlowGraph(structure I=HppaInstr
	    structure P=HppaPseudoOps
           )

structure HppaAsmEmitter = 
  HppaAsmEmitter(structure Instr=HppaInstr
		 structure Shuffle=HppaShuffle
                 structure Stream=HppaStream 
		 structure PseudoOps=HppaPseudoOps)

structure HppaMCEmitter = 
  HppaMCEmitter(structure Instr=HppaInstr
		structure Assembler=HppaAsmEmitter
                structure Stream=HppaStream 
		structure CodeString=CodeString)



