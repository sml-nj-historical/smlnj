(* sparcMLTree.sml --- customize MLRISC for the Sparc.
 * 
 * COPYRIGHT (c) 1998 AT&T Bell Laboratories.
 *)

(* constants specialised to the sparc *)
structure SparcConst = SMLNJConstant

(* specialised sparc instruction set *)
structure SparcInstr = 
  SparcInstr(structure Const = SparcConst
	     structure Region = CPSRegions
(*
             structure Annotations = SMLNJAnnotations
*)
            )

structure SparcPseudoInstrs = SparcPseudoInstrs(SparcInstr)

structure SparcShuffle = SparcShuffle(SparcInstr)

structure SparcPseudoOps = PseudoOpsBig(SparcSpec)

(* flowgraph data structure specialized to Sparc instructions *)
structure SparcFlowGraph = 
  FlowGraph(structure I=SparcInstr
	    structure P=SparcPseudoOps
	    structure B=FunctionNames)

structure SparcAsmEmitter = 
  SparcAsmEmitter(structure Instr=SparcInstr
		  structure Shuffle=SparcShuffle
		  structure PseudoOps=SparcPseudoOps)

structure SparcMCEmitter = 
  SparcMCEmitter(structure Instr=SparcInstr
		 structure Assembler=SparcAsmEmitter
		 structure CodeString=CodeString)

structure SparcMLTree = 
  MLTreeF(structure Const=SparcConst
	  structure R=CPSRegions
	  structure P=SparcPseudoOps
	  structure B=FunctionNames)


(*
 * $Log: sparcMLTree.sml,v $
 * Revision 1.3  1998/12/30 20:21:23  jhr
 *   Modifications to support code generation directly into code objects.
 *
 * Revision 1.2  1998/10/06 14:00:02  george
 * Flowgraph has been removed from modules that do not need it -- [leunga]
 *
 * Revision 1.1.1.1  1998/08/05 19:37:50  george
 *   Release 110.7.4
 *
 *)
