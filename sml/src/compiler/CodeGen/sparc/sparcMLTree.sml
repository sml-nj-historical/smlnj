(* sparcMLTree.sml --- customize MLRISC for the Sparc.
 * 
 * COPYRIGHT (c) 1998 AT&T Bell Laboratories.
 *)

(* constants specialised to the sparc *)
structure SparcConst = SMLNJConstant

(* specialised sparc instruction set *)
structure SparcInstr = 
  SparcInstr(structure Const = SparcConst
	     structure Region = CPSRegions
            )

structure SparcProps = SparcProps(SparcInstr)

structure SparcPseudoInstrs = SparcPseudoInstrs(SparcInstr)

structure SparcShuffle = SparcShuffle(SparcInstr)

structure SparcPseudoOps = PseudoOpsBig(SparcSpec)

(* flowgraph data structure specialized to Sparc instructions *)
structure SparcFlowGraph = 
  FlowGraph(structure I=SparcInstr
	    structure P=SparcPseudoOps
           )

structure SparcStream = InstructionStreamFn(SparcPseudoOps)

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

structure SparcMLTree = 
  MLTreeF(structure Const=SparcConst
	  structure R=CPSRegions
	  structure S=SparcStream
          type rextension = unit
          type fextension = unit
         )


