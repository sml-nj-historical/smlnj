(* hppaMLTree.sml --- customize MLRISC for the HPPA.
 * 
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

(* constants specialised to the hppa *)
structure HppaConst = SMLNJConstant

(* specialised hppa instruction set *)
structure HppaInstr = 
  HppaInstr(structure LabelExp = SMLNJLabelExp
	    structure Region = CPSRegions)

structure HppaShuffle = HppaShuffle(HppaInstr)

structure HppaProps = HppaProps(HppaInstr)

structure HppaPseudoOps = PseudoOpsBig(HppaSpec)

(* flowgraph data structure specialized to Hppa instructions *)
structure HppaFlowGraph = 
  FlowGraph(structure I=HppaInstr
	    structure P=HppaPseudoOps
           )

structure HppaStream = InstructionStream(HppaPseudoOps)

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


structure HppaMLTree = 
  MLTreeF(structure LabelExp=SMLNJLabelExp
	  structure Region=CPSRegions
	  structure Stream=HppaStream
	  structure Extension=SMLNJMLTreeExt
         )

