(* constants specialised to the alpha32 *)

(* specialised alpha32 instruction set *)
structure Alpha32Instr = 
  AlphaInstr(structure Const=SMLNJConstant
             structure Region=CPSRegions)

structure Alpha32Shuffle = AlphaShuffle(Alpha32Instr)

structure Alpha32PseudoInstrs =
  Alpha32PseudoInstrs(structure Instr = Alpha32Instr)

structure Alpha32PseudoOps = 
    PseudoOpsLittle(structure M = Alpha32Spec val nop = NONE)

(* Flowgraph data structure specialized to DEC alpha instructions *)
structure Alpha32FlowGraph = 
  FlowGraph(structure I=Alpha32Instr
	    structure P=Alpha32PseudoOps
	    structure B=FunctionNames)

structure Alpha32Stream = InstructionStreamFn(structure P = Alpha32PseudoOps
                                              structure B = FunctionNames)

structure Alpha32AsmEmitter=
  AlphaAsmEmitter(structure Instr=Alpha32Instr
	          structure PseudoOps=Alpha32PseudoOps
                  structure Stream=Alpha32Stream
		  structure Shuffle = Alpha32Shuffle)

structure Alpha32MCEmitter = 
  AlphaMCEmitter(structure Instr=Alpha32Instr
		 structure PseudoOps=Alpha32PseudoOps
                 structure Stream=Alpha32Stream
		 structure CodeString=CodeString)

(*  Don't need this any more!  
    We now support the alpha32x by generating different instructions.
       -- Allen.

structure Alpha32XMCEmitter = 
  Alpha32XMCEmitter(structure Instr=Alpha32Instr
		   structure PseudoOps=Alpha32PseudoOps
		   structure CodeString=CodeString)
*)


structure Alpha32MLTree = 
  MLTreeF(structure Const=SMLNJConstant
	  structure P=Alpha32PseudoOps
	  structure R=CPSRegions
	  structure B=FunctionNames
          type rextension = unit
          type fextension = unit
         )

(*
 * $Log: alpha32MLTree.sml,v $
 * Revision 1.4  1998/12/30 20:21:21  jhr
 *   Modifications to support code generation directly into code objects.
 *
 * Revision 1.3  1998/10/06 13:59:57  george
 * Flowgraph has been removed from modules that do not need it -- [leunga]
 *
 * Revision 1.2  1998/07/25 03:05:33  george
 *   changes to support block names in MLRISC
 *
 * Revision 1.1.1.1  1998/04/08 18:39:54  george
 * Version 110.5
 *
 *)
