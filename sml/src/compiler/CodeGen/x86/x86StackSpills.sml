signature X86STACKSPILLS = sig
  structure I : X86INSTR
  val init : unit -> unit
  val getRegLoc : int -> I.operand
  val getFregLoc : int -> I.operand
end

structure X86StackSpills : X86STACKSPILLS = 
struct
  exception RegSpills 
  structure I = X86Instr

  fun error msg = ErrorMsg.impossible ("X86CG." ^ msg)

  val initialSpillOffset = X86Runtime.spillStart
  val spillOffset = ref initialSpillOffset
  val spillAreaSz = X86Runtime.spillAreaSz
  fun newOffset n =
    if (n > spillAreaSz) then error "newOffset - spill area is too small"
    else spillOffset := n
  
  val spillTbl : Int32.int Intmap.intmap ref = ref(Intmap.new(0, RegSpills))

  fun init () = 
    (spillOffset:=initialSpillOffset; 
     spillTbl:=Intmap.new(16, RegSpills))

  val toInt32 = Int32.fromInt

  fun getRegLoc (reg:int) = let
    val tbl = !spillTbl
  in
    I.Immed
     (Intmap.map tbl reg
	handle RegSpills => let
	  val offset = !spillOffset
	  val i32 = toInt32 offset
	in 
	  newOffset(offset+4);
	  Intmap.add tbl (reg,i32);
	  i32
	end)
  end

  fun getFregLoc freg = let
    val tbl = !spillTbl
  in
    I.Immed
     (Intmap.map tbl freg
	handle RegSpills => let
	  val offset = !spillOffset
	  val fromInt = Word.fromInt
	  val aligned = Word.toIntX(Word.andb(fromInt offset+0w7, fromInt ~8))
	  val i32 = toInt32 aligned
	in
	  newOffset(aligned+8);
	  Intmap.add tbl (freg, i32);
	  i32
	end)
  end
end
