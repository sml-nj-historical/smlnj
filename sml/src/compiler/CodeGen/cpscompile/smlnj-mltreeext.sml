structure SMLNJMLTreeExt =
struct
   type ('s,'r,'f,'c) sx = unit
   type ('s,'r,'f,'c) rx = unit
   type ('s,'r,'f,'c) fx = unit
   type ('s,'r,'f,'c) ccx = unit
end

functor SMLNJMLTreeExtComp
   (structure T : MLTREE
    structure I : INSTRUCTIONS
      sharing T.LabelExp = I.LabelExp
   ) : MLTREE_EXTENSION_COMP =
struct
   structure T = T
   structure I = I
   structure C = I.C
   type reducer =
     (I.instruction,C.regmap,C.cellset,I.operand,I.addressing_mode) T.reducer
   fun unimplemented _ = MLRiscErrorMsg.impossible "SMLNJMLTreeExtComp" 
   val compileSext  = unimplemented
   val compileRext  = unimplemented
   val compileFext  = unimplemented
   val compileCCext = unimplemented
end
