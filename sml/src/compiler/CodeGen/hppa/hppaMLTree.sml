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
 * Revision 1.6  1998/02/16 13:58:30  george
 *   A register allocated temp is now associated with parallel COPYs
 *   instead of a dedicated register. The temp is used to break cycles.
 *
 * Revision 1.5  1998/02/13 17:21:08  george
 *   Functorized pseudoOps over the machine spec to get access to the
 *   Tag structure.
 *
 * Revision 1.4  1997/09/29 20:58:46  george
 *   Propagate region information through instruction set
 *
# Revision 1.3  1997/07/28  20:05:09  george
#   Added support for regions
#
# Revision 1.2  1997/07/17  12:37:39  george
#   The constant type used to specialize MLTrees is now done more compactly.
#
# Revision 1.1  1997/04/19  18:17:48  george
#   Version 109.27
#
 * Revision 1.1.1.1  1997/01/14  01:38:34  george
 *   Version 109.24
 *
 *)
