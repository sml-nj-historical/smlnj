functor X86MLTreeExtComp
   ( structure T : MLTREE where Extension = X86_SMLNJMLTreeExt
     structure I : X86INSTR where T = T
     structure TS : MLTREE_STREAM
		    where T = T
     structure CFG : CONTROL_FLOW_GRAPH 
		    where I=I 
		      and P = TS.S.P
     val fast_fp : bool ref
   ) : MLTREE_EXTENSION_COMP =
struct
   structure T = T
   structure I = I
   structure C = I.C
   structure CB = CellsBasis
   structure Ext = X86_SMLNJMLTreeExt
   structure CFG = CFG
   structure TS = TS
   structure X86CompInstrExt = 
     X86CompInstrExt
        (structure I=I
	 structure TS = TS
	 structure CFG = CFG)

   type reducer = 
     (I.instruction,C.cellset,I.operand,I.addressing_mode,CFG.cfg) TS.reducer

   fun unimplemented _ = MLRiscErrorMsg.impossible "X86MLTreeExtComp" 

   val compileSext  = X86CompInstrExt.compileSext
   val compileRext  = unimplemented
   val compileCCext = unimplemented
   fun compileFext (TS.REDUCER{reduceFexp, emit, ...}:reducer) = let
     fun comp{e=(64, fexp), fd:CB.cell, an:T.an list} = let
           fun trig(f, foper) = 
	     (reduceFexp f; emit(I.funary foper, an))
         in
	   case fexp
	   of Ext.FSINE f => trig(f, I.FSIN)
	    | Ext.FCOSINE f => trig(f, I.FCOS)
	    | Ext.FTANGENT f => 
	       (trig(f, I.FPTAN); 
		emit(I.fstpl(I.ST(C.ST 0)), [])
               )
	 end
       | comp _ = MLRiscErrorMsg.impossible "compileFext" 

     fun fastComp{e=(64, fexp), fd:CB.cell, an:T.an list} =     
         let fun Freg f = let val fx = CB.registerNum f
                          in  if fx >= 8  andalso fx < 32 (* hardwired! *)
                              then I.FDirect f else I.FPR f 
                          end
             val (unOp, f) =
                     case fexp of
                       Ext.FSINE f => (I.FSIN, f)
                     | Ext.FCOSINE f => (I.FCOS, f)
                     | Ext.FTANGENT f => (I.FPTAN, f)
         in  emit(I.funop{fsize=I.FP64,
                          unOp=unOp,src=Freg(reduceFexp f),dst=Freg fd}, an)
         end
       | fastComp _ = MLRiscErrorMsg.impossible "compileFext"
         
   in if !fast_fp then fastComp else comp
   end
end
