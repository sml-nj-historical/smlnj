functor HppaLabelComp
  (structure MLTree : MLTREE
   structure Instr : HPPAINSTR
     sharing Instr.Constant = MLTree.Constant) = 
struct
  structure T = MLTree
  structure I = Instr
  structure C = I.C
  structure LE = LabelExp

  type reduce = 
    { stm: T.stm -> unit, 
      rexp: T.rexp -> int, 
      emit: I.instruction -> unit }

  datatype lab_opnd = OPND of I.operand | REG of int 

  fun error msg = ErrorMsg.impossible("HppaLabelComp." ^ msg)

  fun ldLabelOpnd emit {label, pref} = OPND(I.LabExp(label,I.T))
  fun ldLabelEA emit lexp = (0, I.LabExp(lexp, I.T))

  (* should change the return pointer to 2 to follow HPUX conventions *)
  fun doCall({stm,rexp,emit}:reduce, T.CALL(exp, def, use)) = let
	fun live([], acc) = acc
	  | live(T.GPR(T.REG r)::regs, acc) = live(regs, C.addReg(r, acc))
	  | live(T.FPR(T.FREG f)::regs, acc) = live(regs, C.addFreg(f, acc))
	  | live(T.CCR(T.CC c)::regs, acc) = live(regs, C.addCCreg(c, acc))
	  | live(_::regs, acc) = live(regs, acc)
	val returnPtr = 31
	val defs = C.addReg(returnPtr, live(def, C.empty))
	val uses = live(use, C.empty)
      in emit(I.BLE{b=rexp exp, d=I.IMMED 0, sr=5, t=returnPtr, defs=defs, uses=uses})
      end
    | doCall _ = error "doCall"

  fun doJmp({stm,rexp,emit}:reduce, T.JMP(exp, labs)) =
    (case exp
     of T.LABEL(LE.LABEL lab) => emit(I.B{lab=lab,n=true})
      | T.LABEL _ => error "doJmp: LABEL"
      | _ => emit(I.BV{b=rexp(exp), x=0, labs=labs, n=true})
    (*esac*))

end
		      

(*
 * $Log: hppaLabelComp.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:55  george
 * Version 110.5
 *
 *)
