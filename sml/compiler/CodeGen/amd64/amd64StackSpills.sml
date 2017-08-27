(* amd64StackSpills.sml
 *
 * COPYRIGHT (c) 2016 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature AMD64STACKSPILLS =
  sig
    structure I : AMD64INSTR
    val init : unit -> unit
    val getRegLoc : int -> I.operand
    val getFregLoc : int -> I.operand
  end

structure AMD64StackSpills : AMD64STACKSPILLS = 
  struct

    structure I = AMD64Instr

    exception RegSpills 

    fun error msg = ErrorMsg.impossible ("AMD64StackSpills." ^ msg)

    val initialSpillOffset = AMD64Runtime.spillStart
    val spillOffset = ref initialSpillOffset
    val spillAreaSz = AMD64Runtime.spillAreaSz
    val availableOffsets = ref [] : I.operand list ref
    val availableFPOffsets = ref [] : I.operand list ref

    fun newOffset n =
          if (n > spillAreaSz)
	    then error "newOffset - spill area is too small"
	    else spillOffset := n
  
    val spillTbl : I.operand IntHashTable.hash_table =
	IntHashTable.mkTable(0, RegSpills)
    val lookupTbl = IntHashTable.lookup spillTbl
    val addTbl    = IntHashTable.insert spillTbl

    fun init () = 
      (spillOffset:=initialSpillOffset; 
       availableOffsets := [];
       availableFPOffsets := [];
       IntHashTable.clear spillTbl
      )

    val toInt32 = Int32.fromInt

    fun getRegLoc reg = 
	lookupTbl reg
	  handle _ => 
	  let val operand = 
	       case !availableOffsets of
		 [] => let val offset = !spillOffset
			   val i32 = toInt32 offset
		       in  newOffset(offset+AMD64Spec.wordByteWidth); I.Immed i32 end
	       | off::offs => (availableOffsets := offs; off) 
	  in addTbl (reg,operand);
	     operand
	  end

    fun getFregLoc freg = 
	lookupTbl freg
	  handle _ => 
	  let val operand = 
	       case !availableFPOffsets of
		 [] =>
		 let val offset = !spillOffset
		     val fromInt = Word.fromInt
		     val aligned = Word.toIntX(Word.andb(fromInt offset+0w7, fromInt ~8))
		 in  newOffset(aligned+8); I.Immed(toInt32 aligned)
		 end
	       | off::offs => (availableFPOffsets := offs; off)
	  in  addTbl (freg, operand);
	      operand
	  end
  end
