(* PseudoOpsBig.sml -- pseudo ops for big endian machines.
 * 
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

functor PseudoOpsBig(M : MACH_SPEC) = struct
  structure T = M.ObjDesc

  fun error msg = ErrorMsg.impossible ("PseudoOpsBig:" ^ msg)

  datatype pseudo_op = 
      MARK
    | REALCONST of Label.label * string
    | STRINGCONST of Label.label * int * string
    | JUMPTABLE of {base:Label.label, targets:Label.label list}


  val >> = Word.>>
  val ~>> = Word.~>>
  val & = Word.andb
  infix >>  ~>>   &
  val itow  = Word.fromInt

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
    fun emitWord w = (emitByte((w >> 0w8) & 0w255); emitByte(w & 0w255))
    fun emitLong n = let
      val w = itow n
    in emitWord(w >> 0w16); emitWord(w & 0w65535)
    end
    fun emitLongX n = let
      val w = itow n
    in emitWord(w ~>> 0w16); emitWord(w & 0w65535)
    end
    fun emitstring s = Word8Vector.app emit (Byte.stringToBytes s)
    fun align loc = case Word.andb(itow(loc), 0w7) 
      of 0w0 => loc
       | 0w4 => (emitLong 0; loc+4)
       | _ => error "align"

    val wtoi =  LargeWord.toInt (* LargeInt.toInt o LargeWord.toLargeIntX *)
    val makeDesc = wtoi o T.makeDesc 
  in
    case pOp
    of MARK => emitLong (makeDesc((loc + 4) div 4, T.tag_backptr))
     | STRINGCONST(_, size, s) => 
         (emitValue{pOp=MARK, loc=loc, emit=emit};
	  emitLong(makeDesc(size, T.tag_string));
	  emitstring s)
     | REALCONST(_, f) => 
	 (emitValue{pOp=MARK, loc= align loc, emit=emit};
	  emitLong(wtoi T.desc_reald);
	  emitstring f)
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
    | sizeOf (JUMPTABLE {targets, ...}, loc) = 4 * length targets

  fun adjustLabels(pOp, loc) = case pOp
    of MARK => ()
     | STRINGCONST(lab, _, _) => Label.setAddr(lab, loc+8)
     | JUMPTABLE{base, ...} => Label.setAddr(base, loc)
     | REALCONST(lab, _) => let
        val aligned = align loc
       in Label.setAddr(lab, aligned+8)
       end
end



	

(*
 * $Log$
 *)
