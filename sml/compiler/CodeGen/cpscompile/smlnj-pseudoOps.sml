(* smlnj-pseudoOps.sml -- pseudo ops for the sml/nj
 * 
 * COPYRIGHT (c) 2001 AT&T Bell Laboratories.
 *
 *)

functor SMLNJPseudoOps
  ( structure Asm : PSEUDO_OPS_BASIS
  ) : SMLNJ_PSEUDO_OPS = 
struct
  structure AsmPseudoOps = Asm
  structure W = Word
  structure PB = PseudoOpsBasisTyp
  structure T = Asm.T

  datatype smlnj_pseudo_op = 
      JUMPTABLE of {base:Label.label,targets:Label.label list}
    | FILENAME of string
  
  type pseudo_op = smlnj_pseudo_op

  fun toBasis(JUMPTABLE{base, targets}) = let
        fun targetOffset t = T.SUB(32, T.LABEL t, T.LABEL base)
        fun pseudoOpOff lab = PB.INT{sz=32, i=[T.LABEXP(targetOffset lab)]}
      in
         PB.ALIGN_SZ 2 ::
           PB.DATA_LABEL base ::
             List.foldr (fn (target, acc) => pseudoOpOff target :: acc) [] targets
      end
    | toBasis(FILENAME file) = let
        fun INT8 n = PB.INT{sz=8, i=[T.LI(T.I.fromInt(8, n))]}
	(* adjust for zero termination and last byte containing the length *)
	val len = Word.fromInt(String.size file) + 0w2
	val K4 = Word.andb(len + 0w3, Word.notb 0w3)
	fun pad 0w0 = [INT8(Word.toInt(Word.>>(K4,0w2)))]
	  | pad n = INT8 (0)::pad(n - 0w1)
      in 
         PB.ALIGN_SZ 2 :: PB.ASCIIZ(file) :: pad (K4-len) 
      end

  fun toString pOp = 
    String.concat(
      List.foldr 
	(fn (p, acc) => AsmPseudoOps.toString p ^ "\n" :: acc) 
	[] (toBasis pOp))

  fun emitValue {pOp, loc, emit} = let
    val pb = toBasis pOp
    fun output(p, loc) = 
	(AsmPseudoOps.emitValue{pOp=p, loc=loc, emit=emit}; 
	 loc + AsmPseudoOps.sizeOf(p, loc))
  in 
     List.foldl output loc (toBasis pOp); ()
  end

  fun sizeOf(pOp,loc) = 
    List.foldl 
	(fn (p, a) => a + AsmPseudoOps.sizeOf(p, loc)) 
	0
	(toBasis pOp)

  fun adjustLabels(JUMPTABLE{base, ...}, loc) = let
	val baseAddr = loc + AsmPseudoOps.sizeOf(PB.ALIGN_SZ 2, loc)
      in
	 if Label.addrOf(base) = baseAddr then false
	 else (Label.setAddr(base, baseAddr); true)
      end
    | adjustLabels(FILENAME _, _) = false

end

