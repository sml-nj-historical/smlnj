(* constants specialised to the alpha32 *)

(* specialised alpha32 instruction set *)
structure Alpha32Instr = 
  AlphaInstr(structure LabelExp=SMLNJLabelExp
             structure Region=CPSRegions)

structure Alpha32Props = AlphaProps(Alpha32Instr)

structure Alpha32Shuffle = AlphaShuffle(Alpha32Instr)

structure Alpha32PseudoOps = 
    PseudoOpsLittle(structure M = Alpha32Spec val nop = NONE)

(* Flowgraph data structure specialized to DEC alpha instructions *)
structure Alpha32FlowGraph = 
  FlowGraph(structure I=Alpha32Instr
	    structure P=Alpha32PseudoOps
           )

structure Alpha32Stream = InstructionStream(Alpha32PseudoOps)

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
  MLTreeF(structure LabelExp=SMLNJLabelExp
	  structure R=CPSRegions
	  structure S=Alpha32Stream
         )

structure Alpha32PseudoInstrs =
  Alpha32PseudoInstrs(structure T = Alpha32MLTree
                      structure Instr = Alpha32Instr)


