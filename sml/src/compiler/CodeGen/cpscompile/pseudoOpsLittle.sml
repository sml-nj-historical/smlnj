(* PseudoOpsLittle.sml -- pseudo ops for the little endian machines
 * 
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

functor PseudoOpsLittle(M: MACH_SPEC) = struct
  structure T = M.ObjDesc

  datatype pseudo_op = 
      MARK
    | REALCONST of Label.label * string
    | STRINGCONST of Label.label * int * string
    | JUMPTABLE of {base:Label.label,targets:Label.label list}

  val >> = Word.>>
  val ~>> = Word.~>>
  val & = Word.andb
  infix >>  ~>>   &
  val itow  = Word.fromInt

  fun error msg = ErrorMsg.impossible ("PseudoOpsLittle:" ^ msg)

  (* since we never compile assembly code, we don't really care
   * about this, but it is good enough for debugging purposes.
   *)
  fun toString (MARK) = ".mark\n"
    | toString (REALCONST(lab, f)) = 
        toString MARK ^ ".real_desc\n" ^ Label.nameOf lab ^
	":\n.double " ^ f ^ "\n"
    | toString (STRINGCONST(lab, _, s)) = 
        toString MARK ^ ".string_desc\n" ^ Label.nameOf lab ^
	":\n.string " ^ s ^ "\n"
    | toString (JUMPTABLE{base, targets}) =
	Label.nameOf base ^ ":\t.jumptable " ^
	List.foldr (op ^) "" (map (fn l => Label.nameOf l ^ " ") targets) ^
	"\n"

  fun removable MARK = true
    | removable _    = false

  fun emitValue{pOp, loc, emit} = let
    fun emitByte n = emit(Word8.fromLargeWord(Word.toLargeWord n))
    fun emitWord n = (emitByte(n & 0w255); emitByte((n >> 0w8) & 0w255))
    fun emitLong n = let 
      val w = itow n
    in emitWord(w & 0w65535);  emitWord(w >> 0w16)
    end
    fun emitLongX n = let 
      val w = itow n
    in emitWord(w & 0w65535);   emitWord(w ~>> 0w16)
    end
    fun emitstring s = Word8Vector.app emit (Byte.stringToBytes s)
    fun align loc = let
      fun fill n = 
	if n>=4 then (emitLong 0; fill(n-4))
	else if n >= 2 then (emitWord 0w0; fill(n-2))
        else if n = 1 then emitByte(0w0) else ()
    in
      case Word.andb(itow(loc), 0w7)
       of 0w0 => loc
        |  w => let
	       val pad = Word.toInt(Word.-(0w8, w))
	     in fill(pad); loc + pad
	     end
    end
    val wtoi =  LargeWord.toInt
    val makeDesc = wtoi o T.makeDesc 
  in
    case pOp
    of MARK => emitLong (makeDesc((loc + 4) div 4, T.tag_backptr))
     | STRINGCONST(_, size, s) =>
         (emitValue{pOp=MARK, loc=loc, emit=emit};
	  emitLong(makeDesc(size, T.tag_string));
	  emitstring s)
     | REALCONST(_, f) =>
	 (emitValue{pOp=MARK, loc=align loc, emit=emit};
	  emitLong(wtoi T.desc_reald);
	  emitstring (implode(rev(explode f))))
     | JUMPTABLE{base, targets} => let
         val baseOff = Label.addrOf base
         fun emitOffset lab = emitLongX(Label.addrOf lab - baseOff)
       in app emitOffset targets
       end
  end

  fun align n = Word.toIntX(Word.andb(0w7+Word.fromInt n, Word.notb 0w7))

  fun sizeOf (MARK, _) = 4
    | sizeOf (STRINGCONST(_, _, s), _) = 8 + size s
    | sizeOf (REALCONST _, loc) = 16 + (align loc - loc)
    | sizeOf (JUMPTABLE {targets, ...}, _) = 4 * length targets

  fun adjustLabels(pOp, loc) = 
    case pOp
    of MARK => ()
     | JUMPTABLE{base, ...} => Label.setAddr(base, loc)
     | STRINGCONST(lab, _, _) => Label.setAddr(lab, loc+8)
     | REALCONST(lab, _) => let 
         val aligned = align loc
       in Label.setAddr(lab, aligned+8)
       end
end



	

(*
 * $Log$
 *)
