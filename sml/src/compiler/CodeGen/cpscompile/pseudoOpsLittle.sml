(* PseudoOpsLittle.sml -- pseudo ops for the little endian machines
 * 
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

functor PseudoOpsLittle
  (structure M: MACH_SPEC 
   val nop : Word8.word option)  = 
struct
  structure T = M.ObjDesc
  structure W = Word

  datatype smlnj_pseudo_op = 
      ALIGN4
    | JUMPTABLE of {base:Label.label,targets:Label.label list}
  
  type pseudo_op = smlnj_pseudo_op

  val >> = Word.>>
  val ~>> = Word.~>>
  val & = Word.andb
  infix >>  ~>>   &
  val itow  = Word.fromInt

  (* since we never compile assembly code, we don't really care
   * about this, but it is good enough for debugging purposes.
   *)
  fun toString (JUMPTABLE{base, targets}) =
	Label.toString base ^ ":\t.jumptable " ^
	List.foldr (op ^) "" (map (fn l => Label.toString l ^ " ") targets) ^
	"\n"
    | toString ALIGN4 = "\t .align\n"

  fun emitValue{pOp, loc, emit} = let
    val itow  = W.fromInt
    fun emitByte n = emit(Word8.fromLargeWord(W.toLargeWord n))
    fun emitWord n = (emitByte(n & 0w255); emitByte((n >> 0w8) & 0w255))
    fun emitLong n = let 
      val w = itow n
    in emitWord(w & 0w65535);  emitWord(w >> 0w16)
    end
    fun emitLongX n = let 
      val w = itow n
    in emitWord(w & 0w65535);   emitWord(w ~>> 0w16)
    end
    fun align(loc) = 
      (case W.andb(itow(loc), 0w3)
       of 0w0 => ()
        |  w => let
	       val noOp = valOf nop
	       val pad = (0w4 - w)
	     in
	       case pad
	        of 0w3 => (emit noOp; emit noOp; emit noOp)
		 | 0w2 => (emit noOp; emit noOp)
		 | 0w1 => (emit noOp)
	       (*esac*)
	     end
      (*esac*))
  in
    case pOp
    of ALIGN4 => align loc
     | JUMPTABLE{base, targets} =>  let
         val baseOff = Label.addrOf base
	 fun emitOffset lab = emitLongX(Label.addrOf lab - baseOff)
       in align loc; app emitOffset targets
       end
  end (* emitValue *)

  local
    (* align on word 4-byte boundary *)
    fun align n = W.toIntX(W.andb(W.fromInt n + 0w3, W.notb 0w3))

    fun padding(loc) = align(loc) - loc
  in
    fun sizeOf(JUMPTABLE {targets, ...}, loc) = 4*length targets + padding(loc)
      | sizeOf(ALIGN4, loc) = padding(loc)

    fun adjustLabels(pOp, loc) = let
      fun setAddr(lab, new) = 
	if Label.addrOf(lab)=new then false else (Label.setAddr(lab, new); true) 
    in
      case pOp
      of JUMPTABLE{base, ...} => setAddr(base, align(loc))
       | ALIGN4 => false
    end
  end
end

