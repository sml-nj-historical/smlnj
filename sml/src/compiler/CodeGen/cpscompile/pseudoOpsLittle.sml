(* PseudoOpsLittle.sml -- pseudo ops for the little endian machines
 * 
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

functor PseudoOpsLittle(M: MACH_SPEC) = struct
  structure T = M.ObjDesc

  datatype pseudo_op = JUMPTABLE of {base:Label.label,targets:Label.label list}

  val >> = Word.>>
  val ~>> = Word.~>>
  val & = Word.andb
  infix >>  ~>>   &
  val itow  = Word.fromInt

  (* since we never compile assembly code, we don't really care
   * about this, but it is good enough for debugging purposes.
   *)
  fun toString (JUMPTABLE{base, targets}) =
	Label.nameOf base ^ ":\t.jumptable " ^
	List.foldr (op ^) "" (map (fn l => Label.nameOf l ^ " ") targets) ^
	"\n"

  fun emitValue{pOp = JUMPTABLE{base, targets}, loc, emit} = let
    fun emitByte n = emit(Word8.fromLargeWord(Word.toLargeWord n))
    fun emitWord n = (emitByte(n & 0w255); emitByte((n >> 0w8) & 0w255))
    fun emitLongX n = let val w = itow n
    in emitWord(w & 0w65535);   emitWord(w ~>> 0w16)
    end
    val baseOff = Label.addrOf base
    fun emitOffset lab = emitLongX(Label.addrOf lab - baseOff)
  in
    app emitOffset targets
  end

  fun align n = Word.toIntX(Word.andb(0w7+Word.fromInt n, Word.notb 0w7))

  fun sizeOf (JUMPTABLE {targets, ...}, _) = 4 * length targets

  fun adjustLabels (JUMPTABLE{base, ...}, loc) = Label.setAddr(base, loc)

end



	

(*
 * $Log: pseudoOpsLittle.sml,v $
 * Revision 1.7  1998/12/21 17:04:56  jhr
 *   Got rid of "removable" function.
 *
 * Revision 1.6  1998/11/18 03:53:10  jhr
 *  New array representations.
 *
 * Revision 1.5  1998/10/28 18:20:41  jhr
 *   Removed code generator support for STRING/REAL constants.
 *
 * Revision 1.4  1998/05/23 14:09:19  george
 *   Fixed RCS keyword syntax
 *
 *)
