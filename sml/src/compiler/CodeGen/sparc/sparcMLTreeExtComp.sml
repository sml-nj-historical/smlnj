functor SparcMLTreeExtComp
   (structure T : MLTREE where Extension = Sparc_SMLNJMLTreeExt
    structure I : SPARCINSTR where T = T) : MLTREE_EXTENSION_COMP =
struct
   structure T = T
   structure I = I
   structure C = I.C
   structure Ext = Sparc_SMLNJMLTreeExt
   structure SparcCompInstrExt = SparcCompInstrExt(I)

   type reducer = 
     (I.instruction,C.cellset,I.operand,I.addressing_mode) T.reducer

   fun unimplemented _ = MLRiscErrorMsg.impossible "SparcMLTreeExtComp" 

   val compileSext  = SparcCompInstrExt.compileSext
   val compileRext  = unimplemented
   val compileCCext = unimplemented
   val compileFext  = unimplemented
end
