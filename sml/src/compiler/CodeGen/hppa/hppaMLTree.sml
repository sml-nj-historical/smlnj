(* hppaMLTree.sml --- customize MLRISC for the HPPA.
 * 
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

(* constants specialised to the hppa *)
structure HppaConst = RegMaskConst(structure RegMask = HppaMask)

(* specialised hppa instruction set *)
structure HppaInstr = 
  HppaInstr(structure Const = HppaConst
	    structure Region = CPSRegions)

structure HppaShuffle = HppaShuffle(HppaInstr)

structure HppaPseudoOps = PseudoOpsBig(HppaSpec)

(* flowgraph data structure specialized to Hppa instructions *)
structure HppaFlowGraph = 
  FlowGraph(structure I=HppaInstr
	    structure P=HppaPseudoOps)

structure HppaAsmEmitter = 
  HppaAsmEmitter(structure Instr=HppaInstr
		 structure Shuffle=HppaShuffle
		 structure FlowGraph=HppaFlowGraph)

structure HppaMCEmitter = 
  HppaMCEmitter(structure Instr=HppaInstr
					structure Assembler=HppaAsmEmitter
		structure FlowGraph=HppaFlowGraph)

structure HppaMLTree = 
  MLTreeF(structure Const=HppaConst
	  structure R=CPSRegions
	  structure P=HppaPseudoOps)


(*
 * $Log: hppaMLTree.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:55  george
 * Version 110.5
 *
 *)
