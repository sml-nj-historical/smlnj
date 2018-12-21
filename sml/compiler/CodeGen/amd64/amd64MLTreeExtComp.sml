(* amd64MLTreeExtComp.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

functor AMD64MLTreeExtComp
   ( structure T : MLTREE where Extension = AMD64_SMLNJMLTreeExt
     structure I : AMD64INSTR where T = T
     structure TS : MLTREE_STREAM
		    where T = T
     structure CFG : CONTROL_FLOW_GRAPH
		    where I=I
		      and P = TS.S.P
   ) : MLTREE_EXTENSION_COMP =
struct
   structure T = T
   structure I = I
   structure C = I.C
   structure CB = CellsBasis
   structure Ext = AMD64_SMLNJMLTreeExt
   structure CFG = CFG
   structure TS = TS
   structure AMD64CompInstrExt =
     AMD64CompInstrExt
        (structure I=I
	 structure TS = TS
	 structure CFG = CFG)

   type reducer =
     (I.instruction,C.cellset,I.operand,I.addressing_mode,CFG.cfg) TS.reducer

   fun unimplemented _ = MLRiscErrorMsg.impossible "AMD64MLTreeExtComp"

   val compileSext  = AMD64CompInstrExt.compileSext
   val compileRext  = unimplemented
   val compileCCext = unimplemented
   fun compileFext (TS.REDUCER{reduceFexp, emit, ...}:reducer) = raise Fail "TODO"
end
