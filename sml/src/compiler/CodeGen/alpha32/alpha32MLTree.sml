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
	    structure P=Alpha32PseudoOps
	    structure B=FunctionNames)

structure Alpha32AsmEmitter=
  Alpha32AsmEmitter(structure Instr=Alpha32Instr
		    structure PseudoOps=Alpha32PseudoOps
		    structure Shuffle = Alpha32Shuffle)

structure Alpha32MCEmitter = 
  Alpha32MCEmitter(structure Instr=Alpha32Instr
		   structure PseudoOps=Alpha32PseudoOps)

structure Alpha32XMCEmitter = 
  Alpha32XMCEmitter(structure Instr=Alpha32Instr
		   structure PseudoOps=Alpha32PseudoOps)


structure Alpha32MLTree = 
  MLTreeF(structure Const=Alpha32Const
	  structure P=Alpha32PseudoOps
	  structure R=CPSRegions
	  structure B=FunctionNames)

(*
 * $Log: alpha32MLTree.sml,v $
 * Revision 1.2  1998/07/25 03:05:33  george
 *   changes to support block names in MLRISC
 *
 * Revision 1.1.1.1  1998/04/08 18:39:54  george
 * Version 110.5
 *
 *)
