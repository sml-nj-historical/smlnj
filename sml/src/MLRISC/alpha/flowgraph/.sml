functor X86GasPseudoOps 
   ( structure T : MLTREE
     structure MLTreeEval : MLTREE_EVAL  where T = T
    ) : PSEUDO_OPS_BASIS = 

struct
  structure T = T
  structure PB = PseudoOpsBasisTyp
  structure Fmt = Format

  structure Endian = 
     PseudoOpsLittle
	 (structure T = T
	  structure MLTreeEval=MLTreeEval
	  val icache_alignment = 16
	  val max_alignment = SOME 7
	  val nop = 0wx90)

  structure GasPseudoOps = 
     GasPseudoOps(structure T = MLTREE
		  val labFmt = {gPrefix="", aPrefix=""})

  type 'a pseudo_op = (T.labexp, 'a) PB.pseudo_op
  
  fun error msg = MLRiscErrorMsg.error ("GasPseudoOps.", msg)

  val sizeOf = Endian.sizeOf
  val valueOf = Endian.valueOf
  val lexpToString = GasPseudoOps.lexpToString
  val toString = GasPseudoOps.toString
end
