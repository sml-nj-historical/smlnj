functor X86MLTreeExtComp
   (structure T : MLTREE  where Extension = SMLNJMLTreeExt
    structure I : X86INSTR
      sharing T.LabelExp = I.LabelExp
   ) : MLTREE_EXTENSION_COMP =
struct
   structure T = T
   structure I = I
   structure C = I.C
   structure Ext = SMLNJMLTreeExt
   type reducer = 
     (I.instruction,C.regmap,C.cellset,I.operand,I.addressing_mode) T.reducer

   fun unimplemented _ = MLRiscErrorMsg.impossible "X86MLTreeExtComp" 

   val compileSext  = unimplemented
   val compileRext  = unimplemented
   val compileCCext = unimplemented
   fun compileFext (T.REDUCER{reduceFexp, emit, ...}:reducer) = let
     fun comp{e=(64, fexp), fd:C.cell, an:T.an list} = let
           val fpTmpMem  =  X86Runtime.fpTempMemOff
           fun trig(f, foper) = 
	     (reduceFexp f; emit(I.FUNARY foper, an))
         in
	   case fexp
	   of Ext.FSINE f => trig(f, I.FSIN)
	    | Ext.FCOSINE f => trig(f, I.FCOS)
	    | Ext.FTANGENT f => 
	       (trig(f, I.FTAN); 
		emit(I.FSTPL(I.Displace{base=C.esp,
					disp=I.Immed(fpTmpMem), 
					mem=I.Region.stack}),
		     []))
	       
	 end
       | comp _ = MLRiscErrorMsg.impossible "compileFext" 
   in comp
   end
end
