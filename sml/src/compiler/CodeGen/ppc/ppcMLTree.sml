(* COPYRIGHT (c) 1999 Lucent Technologies, Bell Labs. *)

(* constants specialised to the powerpc *)
structure PPCConst = RegMaskConst(structure RegMask=PPCMask)

(* specialised powerpc instruction set *)
structure PPCInstr = 
  PPCInstr(structure Const=PPCConst
	   structure Region=CPSRegions)

structure PPCShuffle = PPCShuffle(PPCInstr)

structure PPCPseudoOps = PseudoOpsBig(PPCSpec)

(* Flowgraph data structure specialized to DEC alpha instructions *)
structure PPCFlowGraph = 
  FlowGraph(structure I=PPCInstr
	    structure P=PPCPseudoOps
	    structure B=FunctionNames)

structure PPCAsmEmitter=
  PPCAsmEmitter(structure Instr=PPCInstr
		structure PseudoOps=PPCPseudoOps
		structure Shuffle = PPCShuffle)

structure PPCMCEmitter = 
  PPCMCEmitter(structure Instr=PPCInstr
	       structure PseudoOps=PPCPseudoOps
	       structure CodeString=CodeString)


structure PPCMLTree = 
  MLTreeF(structure Const=PPCConst
	  structure P=PPCPseudoOps
	  structure R=CPSRegions
	  structure B=FunctionNames)
