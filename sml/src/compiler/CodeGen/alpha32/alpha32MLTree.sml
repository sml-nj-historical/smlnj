(* constants specialised to the alpha32 *)
structure Alpha32Const = RegMaskConst(structure RegMask=Alpha32Mask)

(* specialised alpha32 instruction set *)
structure Alpha32Instr = 
  Alpha32Instr(structure Const=Alpha32Const
	       structure Region=CPSRegions)

structure Alpha32Shuffle = Alpha32Shuffle(Alpha32Instr)

structure Alpha32PseudoInstrs =
  Alpha32PseudoInstrs(structure Instr = Alpha32Instr)

structure Alpha32PseudoOps = PseudoOpsLittle(Alpha32Spec)

(* Flowgraph data structure specialized to DEC alpha instructions *)
structure Alpha32FlowGraph = 
  FlowGraph(structure I=Alpha32Instr
	    structure P=Alpha32PseudoOps)

structure Alpha32AsmEmitter=
  Alpha32AsmEmitter(structure Instr=Alpha32Instr
		    structure FlowGraph=Alpha32FlowGraph
		    structure Shuffle = Alpha32Shuffle)

structure Alpha32MCEmitter = 
  Alpha32MCEmitter(structure Instr=Alpha32Instr
		   structure FlowGraph=Alpha32FlowGraph)

structure Alpha32XMCEmitter = 
  Alpha32XMCEmitter(structure Instr=Alpha32Instr
		   structure FlowGraph=Alpha32FlowGraph)


structure Alpha32MLTree = 
  MLTreeF(structure Const=Alpha32Const
	  structure P=Alpha32PseudoOps
	  structure R=CPSRegions)

(*
 * $Log: alpha32MLTree.sml,v $
 * Revision 1.6  1998/02/16 13:58:24  george
 *   A register allocated temp is now associated with parallel COPYs
 *   instead of a dedicated register. The temp is used to break cycles.
 *
 * Revision 1.5  1998/02/13 17:20:49  george
 *   Functorized pseudoOps over the machine spec to get access to the
 *   Tag structure.
 *
 * Revision 1.4  1997/08/29 11:03:20  george
 *   Instruction selection is now parameterised over pseudo instructions.
 *
# Revision 1.3  1997/07/28  20:04:33  george
#   Added support for regions
#
# Revision 1.2  1997/07/17  12:35:00  george
#   The constant type used to specialize MLTrees is now done more compactly.
#
# Revision 1.1  1997/04/19  18:17:43  george
#   Version 109.27
#
 * Revision 1.1.1.1  1997/01/14  01:38:33  george
 *   Version 109.24
 *
 *)
