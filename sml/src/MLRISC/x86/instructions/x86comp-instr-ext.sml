(* x86comp-instr-ext.sml
 *
 * COPYRIGHT (c) 2000 Bell Labs, Lucent Technologies
 *
 * emit code for extensions to the x86 instruction set.
 *)

signature X86COMP_INSTR_EXT = sig
  structure T : MLTREE
  structure I : X86INSTR

  type reducer = 
    (I.instruction, I.C.regmap, I.C.cellset, I.operand, I.addressing_mode) T.reducer

  val compileSext : 
     reducer -> {stm: (T.rexp, T.fexp) X86InstrExt.sext, an: T.an list} -> unit
end


functor X86CompInstrExt
  (structure T : MLTREE 
   structure I : X86INSTR 
     sharing T.LabelExp = I.LabelExp) : X86COMP_INSTR_EXT = 
struct
  structure T = T
  structure I = I
  structure C = I.C
  structure X = X86InstrExt

  type stm = (T.rexp, T.fexp) X.sext

  type reducer = 
    (I.instruction, I.C.regmap, I.C.cellset, I.operand, I.addressing_mode) T.reducer

  val esp = C.esp
  val espOpnd = I.Direct(esp)

  fun error msg = MLRiscErrorMsg.error("X86CompInstrExt", msg)

  val stackArea = I.Region.stack

  fun compileSext reducer {stm: stm, an:T.an list} = let
    val T.REDUCER{operand, emit, reduceFexp, instrStream, ...} = reducer
    val T.Stream.STREAM{emit=emitI, ...} = instrStream
  in
    case stm
    of X.PUSHL(rexp) => emit(I.PUSHL(operand rexp), an)
     | X.PUSHf{sz, fexp} => let
	 fun inRange f = let
	   val {high, low} = C.cellRange C.FP
	 in low <= f andalso f <= high
	 end

	 fun alloc(size) = 
	   emitI(I.BINARY{binOp=I.SUBL, src=I.Immed size, dst=espOpnd});

	 fun copyf(f, fld, fstp, size) = 
	   (emitI(fld(I.FDirect f));
	    alloc(size);
	    emitI(I.BINARY{binOp=I.ADDL, src=I.Immed size, dst=espOpnd});
	    emit(fstp(I.Displace{base=esp, disp=I.Immed(0), mem=stackArea}), an))
       in
	 case fexp 
	 of T.FREG(_, f) => 
	    if inRange(f) then error "compileSext: FREG: inRange"
	    else (case sz
	      of X.single => copyf(f, I.FLDS, I.FSTPS, 4)
	       | X.double => copyf(f, I.FLDL, I.FSTPL, 8)
	       | X.extended => error "compileSext: FREG: sz=80"
	      (*esac*))
         | _ => let
	      val f = reduceFexp(fexp)
	      fun pushf(size, fstp) = 
		(alloc(size); 
		 emit(fstp(I.Displace{base=esp, disp=I.Immed(0), mem=stackArea}), an))
	    in 
	       case sz
	       of X.single => pushf(4, I.FSTPS)
                | X.double => pushf(8, I.FSTPL)
                | X.extended => error "compileSext: fexp: sz=80"
	    end
	(*esac*)
       end
  end
end
