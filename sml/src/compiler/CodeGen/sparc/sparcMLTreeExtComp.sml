functor SparcMLTreeExtComp
   (structure T : MLTREE 
   		where Extension = Sparc_SMLNJMLTreeExt
    structure I : SPARCINSTR
    		where T = T
    structure CFG : CONTROL_FLOW_GRAPH 
    		where I = I
		  and P = T.PseudoOp) : MLTREE_EXTENSION_COMP =
struct
   structure T = T
   structure I = I
   structure C = I.C
   structure Ext = Sparc_SMLNJMLTreeExt
   structure CFG = CFG
   structure SparcCompInstrExt = 
     SparcCompInstrExt(structure I = I structure CFG = CFG)

   type reducer = 
     (I.instruction,C.cellset,I.operand,I.addressing_mode, CFG.cfg) T.reducer

   fun unimplemented _ = MLRiscErrorMsg.impossible "SparcMLTreeExtComp" 

   val compileSext  = SparcCompInstrExt.compileSext
   val compileRext  = unimplemented
   val compileCCext = unimplemented
   val compileFext  = unimplemented
end
