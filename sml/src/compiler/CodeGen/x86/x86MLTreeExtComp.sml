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

   val fast_fp = MLRiscControl.getFlag "x86-fast-fp"

   fun unimplemented _ = MLRiscErrorMsg.impossible "X86MLTreeExtComp" 

   val compileSext  = unimplemented
   val compileRext  = unimplemented
   val compileCCext = unimplemented
   fun compileFext (T.REDUCER{reduceFexp, emit, ...}:reducer) = let
     fun comp{e=(64, fexp), fd:C.cell, an:T.an list} = let
           fun trig(f, foper) = 
	     (reduceFexp f; emit(I.FUNARY foper, an))
         in
	   case fexp
	   of Ext.FSINE f => trig(f, I.FSIN)
	    | Ext.FCOSINE f => trig(f, I.FCOS)
	    | Ext.FTANGENT f => 
	       (trig(f, I.FPTAN); 
		emit(I.FSTPL(I.ST(C.ST 0)), [])
               )
	 end
       | comp _ = MLRiscErrorMsg.impossible "compileFext" 

     fun fastComp{e=(64, fexp), fd:C.cell, an:T.an list} =     
         let fun Freg f = if f >= 32+8 andalso f < 32+32 (* hardwired! *)
                          then I.FDirect f else I.FPR f 
             val (unOp, f) =
                     case fexp of
                       Ext.FSINE f => (I.FSIN, f)
                     | Ext.FCOSINE f => (I.FCOS, f)
                     | Ext.FTANGENT f => (I.FPTAN, f)
         in  emit(I.FUNOP{fsize=I.FP64,
                          unOp=unOp,src=Freg(reduceFexp f),dst=Freg fd}, an)
         end
       | fastComp _ = MLRiscErrorMsg.impossible "compileFext"
         
   in if !fast_fp then fastComp else comp
   end
end
