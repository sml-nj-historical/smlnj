(* COPYRIGHT (c) 1999 Lucent Technologies, Bell Labs. *)

(* constants specialised to the powerpc *)
structure PPCConst = SMLNJConstant

(* specialised powerpc instruction set *)
structure PPCInstr = 
  PPCInstr(structure LabelExp=SMLNJLabelExp
	   structure Region=CPSRegions)

structure PPCProps = PPCProps(PPCInstr)

structure PPCShuffle = PPCShuffle(PPCInstr)

structure PPCPseudoOps = PseudoOpsBig(PPCSpec)

(* Flowgraph data structure specialized to DEC alpha instructions *)
structure PPCFlowGraph = 
  FlowGraph(structure I=PPCInstr
	    structure P=PPCPseudoOps
           )

structure PPCStream = InstructionStream(PPCPseudoOps)

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


structure PPCMLTree = 
  MLTreeF(structure LabelExp=SMLNJLabelExp
	  structure R=CPSRegions
	  structure S=PPCStream
         )
