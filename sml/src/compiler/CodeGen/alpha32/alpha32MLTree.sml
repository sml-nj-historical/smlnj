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


structure Alpha32MLTree = 
  MLTreeF(structure Const=SMLNJConstant
	  structure P=Alpha32PseudoOps
	  structure R=CPSRegions
	  structure B=FunctionNames
          type rextension = unit
          type fextension = unit
         )

