(* X86.sml -- pattern matching version of x86 instruction set generation. 
 *
 * COPYRIGHT (c) 1998 Bell Laboratories.
 *
 *)
functor X86
  (structure X86Instr : X86INSTR
   structure X86MLTree : MLTREE
     where Region = X86Instr.Region
       and Constant = X86Instr.Constant
   structure Flowgen : FLOWGRAPH_GEN
     where I = X86Instr  and T = X86MLTree and B = X86MLTree.BNames
   val tempMem : X86Instr.operand) : MLTREECOMP = 
struct
  structure F = Flowgen
  structure T = X86MLTree
  structure I = X86Instr
  structure C = X86Cells

  structure W32 = Word32
  structure LE = LabelExp

  fun error msg = MLRiscErrorMsg.impossible ("X86." ^ msg)

  (* label where a trap is generated -- one per cluster *)
  val trapLabel = ref (NONE: Label.label option)

  val emitInstr = F.emitInstr
  val emit    = F.emitInstr
  val newReg  = C.newReg
  val newFreg = C.newFreg

  (* conversions *)
  val itow = Word.fromInt
  val wtoi = Word.toInt
  val toInt32 = Int32.fromLarge o Int.toLarge

  (* One day, this is going to bite us when precision(LargeInt)>32 *)
  val wToInt32 = Int32.fromLarge o Word32.toLargeIntX

  (* some useful registers *)
  val eax = I.Direct(C.eax)
  val ecx = I.Direct(C.ecx)
  val edx = I.Direct(C.edx)

  fun immed8 n = Int32.>=(n, ~256) andalso Int32.<(n,256) 
  fun immedLabel lab = I.ImmedLabel(LE.LABEL lab)

  fun move(src as I.Direct s, dst as I.Direct d) = 
      if s=d then () 
      else emit(I.COPY{dst=[d], src=[s], tmp=NONE})
    | move(src, dst) =  emit(I.MOVE{mvOp=I.MOVL, src=src, dst=dst})

  fun moveToReg opnd = let
    val dst = I.Direct(newReg())
  in move(opnd, dst); dst
  end

  (* ensure that the operand is either an immed or register *)
  fun immedOrReg opnd = 
    case opnd
     of I.Displace _ => moveToReg opnd
      | I.Indexed _ =>  moveToReg opnd
      | _  => opnd
    (*esac*)

  fun isImmediate(I.Immed _) = true
    | isImmediate(I.ImmedLabel _) = true
    | isImmediate(I.Const _) = true
    | isImmediate(I.LabelEA _) = true
    | isImmediate _ = false

  fun regOrMem opnd = if isImmediate opnd then moveToReg opnd else opnd


fun rexp(T.REG r) = ["r" ^ Int.toString r]
  | rexp(T.LI i)  = ["LI"]
  | rexp(T.LI32 i32) = ["LI32"]
  | rexp(T.LABEL  le) = ["LABEL"]
  | rexp(T.CONST  c) = ["CONST"]

  | rexp(T.ADD  (e1, e2)) = ["ADD("] @ rexp e1 @ (","::rexp e2) @ [")"]
  | rexp(T.SUB  (e1, e2, _)) = ["SUB"]
  | rexp(T.MULU (e1, e2)) = ["MULU"]
  | rexp(T.DIVU   (e1, e2, _)) =  ["DIVU"]

  | rexp(T.ADDT  (e1, e2)) =   ["ADDT"]
  | rexp(T.MULT   (e1, e2)) =  ["MULT"]
  | rexp(T.SUBT    (e1, e2, _)) = ["SUBT"]
  | rexp(T.DIVT    (e1, e2, _)) = ["DIVT"]

  | rexp(T.LOAD8  (e, _)) = ["LOAD8("] @ rexp e @ [")"]
  | rexp(T.LOAD32  (e, _)) = ["LOAD32"]

  | rexp(T.ANDB   (e1, e2)) =  ["AND"]
  | rexp(T.ORB     (e1, e2)) = ["OR"]
  | rexp(T.XORB    (e1, e2)) = ["XOR"]

  | rexp(T.SRA    (e1, e2, _)) = ["SRA("] @ rexp e1 @ (","::rexp e2) @ [")"]
  | rexp(T.SRL    (e1, e2, _)) = ["SRL"]
  | rexp(T.SLL    (e1, e2, _)) = ["SLL"]

  | rexp(T.SEQ(s, e)) = ["SEQ("] @ stm s @ ("," :: rexp e) @ [")"]

and stm s = 
 (case s 
  of T.MV(r, e) => ["MV(", Int.toString r] @ (",":: rexp e) @ [")"]
   | T.FMV _ => ["FMV"] 
   | T.COPY _  => ["COPY"]
   | T.FCOPY _ => ["FCOPY"]
   | T.JMP _ => ["JMP"]
   | T.CALL _ => ["CALL"]
   | T.RET  => ["RET"]
   | T.STORE8 _ => ["STORE8"]
   | T.STORE32 _ => ["STORE32"]
   | T.STORED _ => ["STORED"]
   | T.STORECC _ => ["STORECC"]
   | T.BCC    _ => ["BCC"]
   | T.FBCC   _ => ["FBCC"]
  (*esac*))

fun prMLRisc s = print(concat(stm s))



  exception EA

  (* return an index computation *)
  fun index(arg as (T.SLL(t, T.LI n, _))) =
      if n > 0 andalso n <= 3 then {index=reduceReg t, scale=n}
      else {index=reduceReg arg, scale=0}
    | index t = {index=reduceReg t, scale=0}

  (* return effective address *)
  and ea eatree = let
    (* Need to ensure that the scale register is never %esp *)
    fun doImmed(n, I.Immed(k)) = (I.Immed(k+toInt32 n) handle Overflow => raise EA)
      | doImmed(n, I.ImmedLabel le) = I.ImmedLabel(LE.PLUS(le, LE.CONST n))
      | doImmed(n, I.Const c) = 
          I.Displace{base=reduceReg(T.CONST c), disp=I.Immed(toInt32 n)}

    fun doConst(c, I.Immed(0)) = I.Const c
      | doConst(c, d) = I.Displace{base=reduceReg(T.CONST c), disp=d} 

    fun doLabel(le, I.Immed(0)) = I.ImmedLabel le
      | doLabel(le, I.Immed(n)) = I.ImmedLabel(LE.PLUS(le, LE.CONST(Int32.toInt n)))
      | doLabel(le, I.Const c) = 
          I.Displace{base=reduceReg(T.CONST c), disp=I.ImmedLabel le}

    fun newDisp(n, combine, I.Displace{base, disp}) = 
          I.Displace{base=base, disp=combine(n, disp)}
      | newDisp(n, combine, I.Indexed{base, index, scale, disp}) = 
	  I.Indexed{base=base, index=index, scale=scale, disp=combine(n, disp)}
      | newDisp(n, combine, disp) = combine(n, disp)

    fun combineBase(tree, base) = 
      SOME(case base
	   of NONE => reduceReg tree
	    | SOME base => reduceReg(T.ADD(T.REG base, tree))
	   (*esac*))

    (* keep building a bigger and bigger effective address expressions *)
    fun doEA(T.LI n, mode) = newDisp(n, doImmed, mode)
      | doEA(T.LABEL le, mode) = newDisp(le, doLabel, mode)
      | doEA(T.CONST c, mode) = newDisp(c, doConst, mode)
      | doEA(t0 as T.SLL(t, T.LI scale, _), mode) = 
        if scale >= 1 andalso scale <= 3 then 
	 (case mode
	  of I.Displace{base, disp} =>
 	      I.Indexed
	        {base=SOME base, index=reduceReg t, scale=scale, disp=disp}
	   | I.Indexed{base, index, scale, disp} => 
	      I.Indexed{base=combineBase(t0,base), 
			index=index, scale=scale, disp=disp}
	   | disp => 
	      I.Indexed{base=NONE, index=reduceReg t, scale=scale, disp=disp}
	 (*esac*))
	else
	 (case mode
	  of I.Displace{base, disp} => 
	       I.Displace{base=Option.valOf(combineBase(t0, SOME base)), disp=disp}
	   | I.Indexed{base, index, scale, disp} => 
	       I.Indexed{base=combineBase(t0, base), 
			 index=index, scale=scale, disp=disp}
	   | disp => I.Displace{base=reduceReg(t0), disp=disp}
	 (*esac*))
      | doEA(T.ADD(t1, t2 as T.REG _), mode) = doEA(t1, doEA(t2, mode))
      | doEA(T.ADD(t1, t2), mode) = doEA(t2, doEA(t1, mode))
      | doEA(T.SUB(t1, T.LI n, _), mode) = doEA(T.ADD(t1, T.LI (~n)), mode)
      | doEA(t, I.Indexed{base, index, scale, disp}) = 
	  I.Indexed{base=combineBase(t, base), index=index, scale=scale, disp=disp}
      | doEA(T.REG r, I.Displace{base, disp}) = 
	  I.Indexed{base=SOME base, index=r, scale=0, disp=disp}
      | doEA(t, I.Displace{base, disp}) =
	  I.Indexed{base=SOME base, index=reduceReg t, scale=0, disp=disp}
      | doEA(t, immed) = I.Displace{base=reduceReg t, disp=immed}
  in 
    case doEA(eatree, I.Immed 0)
    of I.Immed _ => raise EA
     | I.ImmedLabel le => I.LabelEA le
     | ea => ea
  end (* ea *)

  and operand(T.LI i) = I.Immed(toInt32 i)
    | operand(T.LI32 w) = I.Immed(wToInt32 w)
    | operand(T.CONST c) = I.Const c
    | operand(T.LABEL lab) = I.ImmedLabel lab
    | operand(T.REG r) = I.Direct r
    | operand(T.LOAD32(t, _)) = ea t
    | operand(t) = I.Direct(reduceReg(t))

  (* operand with preferred target *)
  and operandRd(T.LI i, _) = I.Immed (toInt32 i)
    | operandRd(T.LI32 w, _) = I.Immed(wToInt32 w)
    | operandRd(T.REG r, _)  = I.Direct r
    | operandRd(T.LOAD32(t,_), _) = ea t
    | operandRd(t, rd) = I.Direct(reduceRegRd(t, rd))

  (* evaluate left-to-right or right-to-left *)
  and ordered(e1, e2, T.LR) = (operand e1, operand e2)
    | ordered(e1, e2, T.RL) = let 
	val opnd2 = operand e2
      in (operand e1, opnd2)
      end

  and cond T.LT = I.LT	| cond T.LTU = I.B
    | cond T.LE = I.LE	| cond T.LEU = I.BE
    | cond T.EQ = I.EQ	| cond T.NEQ = I.NE
    | cond T.GE = I.GE	| cond T.GEU = I.AE
    | cond T.GT = I.GT	| cond T.GTU = I.A

 (* reduce an MLRISC statement tree *)
  and reduceStm(T.MV(rd, exp)) = let
        fun mv src = emit(I.MOVE{mvOp=I.MOVL, src=src, dst=I.Direct rd})
      in
        case operandRd(exp, rd)
	 of opnd as I.Direct rd' => if rd'=rd then () else mv opnd
          | opnd => mv opnd
      end
    | reduceStm(T.FMV(fd, T.FREG fs)) = 
       if fs=fd then () else emit(I.COPY{dst=[fd], src=[fs], tmp=NONE})
    | reduceStm(T.FMV(fd, T.LOADD(t, _))) =
       (emit(I.FLD(ea t)); emit(I.FSTP(I.FDirect fd)))
    | reduceStm(T.FMV(fd, e)) = (reduceFexp e; emit(I.FSTP(I.FDirect fd)))
    | reduceStm(T.CCMV(0, exp)) = reduceCC(exp, 0)
    | reduceStm(T.CCMV _) = error "reduceStm: CCMV"
    | reduceStm(T.COPY(dst as [_], src)) = 
        emit(I.COPY{dst=dst, src=src, tmp=NONE})
    | reduceStm(T.COPY(dst, src)) = 
        emit(I.COPY{dst=dst, src=src, tmp=SOME(I.Direct(newReg()))})
    | reduceStm(T.FCOPY(dst, src)) = 
	emit(I.FCOPY{dst=dst, src=src, tmp=SOME(I.FDirect(newFreg()))})
    | reduceStm(T.JMP(T.LABEL lexp, labs)) = emit(I.JMP(I.ImmedLabel lexp, labs))
    | reduceStm(T.JMP(exp, labs)) = emit(I.JMP (operand exp, labs))
    | reduceStm(T.CALL(t,def,use)) = let
       val addCCreg = C.addCell C.CC
       fun addList([], acc) = acc
	 | addList(T.GPR(T.REG r)::regs, acc) = addList(regs, C.addReg(r, acc))
	 | addList(T.FPR(T.FREG r)::regs, acc) = addList(regs, C.addFreg(r, acc))
	 | addList(T.CCR(T.CC cc)::regs, acc) = addList(regs, addCCreg(cc, acc))
	 | addList(_::regs, acc) = addList(regs, acc)
      in
	emit(I.CALL(operand t, addList(def,C.empty), addList(use,C.empty)))
      end
    | reduceStm(T.RET) = emit(I.RET)
    | reduceStm(T.STORE8(t1, t2, ord)) = let
       val opnd = immedOrReg(operand t2)
       val src = 
	 (case opnd
	  of I.Direct r => if r = C.eax then opnd else (move(opnd, eax); eax)
	   | _ => opnd
	  (*esac*))
      in emit(I.MOVE{mvOp=I.MOVB, src=src, dst=ea t1})
      end	  
    | reduceStm(T.STORE32(t1, t2, _)) = move(immedOrReg(operand t2), ea t1)
    | reduceStm(T.STORED(t1, t2, _)) = 
       (case t2 
	 of T.FREG fs => emit(I.FLD(I.FDirect fs))
          | e => reduceFexp e
        (*esac*);
        emit(I.FSTP(ea t1)))
    | reduceStm(T.STORECC _) = error "stmAction: STORECC"
    | reduceStm(T.BCC(_, T.CMP(cc as (T.EQ | T.NEQ), t1, T.LI 0, _), lab)) = let
	val opnd1 = operand t1
	fun jcc() = emit(I.JCC{cond=cond cc, opnd=immedLabel lab})
      in
	case t1
	of T.ANDB _ => jcc()
         | T.ORB _ =>  jcc()
	 | T.XORB _ => jcc()
	 | T.SRA _ =>  jcc()
	 | T.SRL _ =>  jcc()
	 | T.SLL _ =>  jcc()
	 | _ => (emit(I.CMP{lsrc=opnd1, rsrc=I.Immed 0}); jcc())
      end		 
    | reduceStm(T.BCC(_, T.CMP(cc, t1, t2, ord), lab)) = let
	fun swapcc T.LT = T.GT	| swapcc T.LTU = T.GTU
          | swapcc T.LE = T.GE	| swapcc T.LEU = T.GEU
	  | swapcc T.EQ = T.EQ	| swapcc T.NEQ = T.NEQ
	  | swapcc T.GE = T.LE	| swapcc T.GEU = T.LEU
	  | swapcc T.GT = T.LT	| swapcc T.GTU = T.LTU

	fun cmpAndBranch(cc, opnd1, opnd2) = 
	  (emit(I.CMP{lsrc=opnd1, rsrc=opnd2});
	   emit(I.JCC{cond=cond cc, opnd=immedLabel lab}))


        val (opnd1, opnd2) = ordered(t1, t2, ord)
      in
	if isImmediate opnd1 andalso isImmediate opnd2 then
	  cmpAndBranch(cc, moveToReg opnd1, opnd2)
	else if isImmediate opnd1 then
	  cmpAndBranch(swapcc cc, opnd2, opnd1)
	else if isImmediate opnd2 then
	  cmpAndBranch(cc, opnd1, opnd2)
	else case (opnd1, opnd2)
	 of (_, I.Direct _) => cmpAndBranch(cc, opnd1, opnd2)
          | (I.Direct _, _) => cmpAndBranch(cc, opnd1, opnd2)
	  | _ => cmpAndBranch(cc, moveToReg opnd1, opnd2)
         (*esac*)
      end
    | reduceStm(T.BCC(cc, T.CC(0), lab)) = 
        emit(I.JCC{cond=cond cc, opnd=immedLabel lab})
    | reduceStm(T.BCC _) = error "reduceStm: BCC"
    | reduceStm(T.FBCC(_, T.FCMP(fcc, t1, t2, ord), lab)) = let
	fun compare() = let
	  fun ignoreOrder (T.FREG _) = true
	    | ignoreOrder (T.LOADD _) = true
	    | ignoreOrder _ = false
	  fun t2t1 () = (reduceFexp t2; reduceFexp t1)
        in
	  if ignoreOrder t1 orelse ignoreOrder t2 then t2t1()
	  else 
	    (case ord
	      of T.RL => t2t1()
	       | T.LR => (reduceFexp t1; reduceFexp t2; emit(I.FXCH))
	    (*esac*));
          emit(I.FUCOMPP)
        end
	fun branch() = let
	  val eax = I.Direct C.eax
	  fun andil i = emit(I.BINARY{binOp=I.AND, src=I.Immed(i), dst=eax})
	  fun xoril i = emit(I.BINARY{binOp=I.XOR, src=I.Immed(i), dst=eax})
	  fun cmpil i = emit(I.CMP{rsrc=I.Immed(i), lsrc=eax})
	  fun j(cc, lab) = emit(I.JCC{cond=cc, opnd=immedLabel lab})
	  fun sahf() = emit(I.SAHF)
	in
	  case fcc
	  of T.==   => (andil 0x4400; xoril 0x4000; j(I.EQ, lab))
           | T.?<>  => (andil 0x4400; xoril 0x4000; j(I.NE, lab))
	   | T.?    => (sahf(); j(I.P,lab))
	   | T.<=>  => (sahf(); j(I.NP,lab))
	   | T.>    => (andil 0x4500;  j(I.EQ,lab))
	   | T.?<=  => (andil 0x4500;  j(I.NE,lab))
	   | T.>=   => (andil 0x500; j(I.EQ,lab))
	   | T.?<   => (andil 0x500; j(I.NE,lab))
	   | T.<    => (andil 0x4500; cmpil 0x100; j(I.EQ,lab))
	   | T.?>=  => (andil 0x4500; cmpil 0x100; j(I.NE,lab))
	   | T.<=   => (andil 0x4100; cmpil 0x100; j(I.EQ,lab);
			cmpil 0x4000; j(I.EQ,lab))
	   | T.?>   => (sahf(); j(I.P,lab); andil 0x4100; j(I.EQ,lab))
	   | T.<>   => (andil 0x4400; j(I.EQ,lab))
	   | T.?=   => (andil 0x4400; j(I.NE,lab))
	 (*esac*)
	end
      in compare(); emit I.FNSTSW; branch() 
      end
    | reduceStm(T.FBCC _) = error "reduceStm: FBCC"

  and reduceCC(T.CMP(_, t1, t2, ord), 0) = let
	val (opnd1, opnd2) = ordered(t1, t2, ord)
      in 
	emit(I.CMP(
	  case (opnd1, opnd2)
	  of (I.Immed _, _) => {lsrc=moveToReg opnd1, rsrc=opnd2}
	   | (I.ImmedLabel _, _) => {lsrc=moveToReg opnd1, rsrc=opnd2}
	   | (I.Const _, _) => {lsrc=moveToReg opnd1, rsrc=opnd2}
	   | (I.Direct _, _) => {lsrc=opnd1, rsrc=opnd2}
	   | (_, I.Direct _) => {lsrc=opnd1, rsrc=opnd2}
	   | _ => {lsrc=moveToReg opnd1, rsrc=opnd2}))
      end
    | reduceCC _ = error "reduceCC" 


  and reduceReg(T.REG rd) = rd
    | reduceReg(exp) = reduceRegRd(exp, newReg())

 (* reduce to the register rd where possible.*)
  and reduceRegRd(exp, rd) = let
    val opndRd = I.Direct(rd)

    fun binary(comm, oper, e1, e2, order) = let
      fun emit2addr (opnd1, opnd2) = 
	(move(opnd1, opndRd);
	 emit(I.BINARY{binOp=oper, dst=opndRd, src=opnd2});
	 rd)
      fun commute(opnd1 as I.Immed _, opnd2) = (opnd2, opnd1)
	| commute(opnd1 as I.ImmedLabel _, opnd2) = (opnd2, opnd1)
	| commute(opnd1 as I.Const _, opnd2) = (opnd2, opnd1)
	| commute(opnd1, opnd2 as I.Direct _) = (opnd2, opnd1)
	| commute arg = arg

      val opnds = ordered(e1, e2, order)
    in emit2addr(if comm then commute opnds else opnds)
    end (*binary*)

    fun unary(oper, exp) = 
      (move(operand exp, opndRd);
       emit(I.UNARY{unOp=oper, opnd=opndRd});
       rd)

   (* The shift count can be either an immediate or the ECX register *)
    fun shift(oper, e1, e2, order) = let
      val (opnd1, opnd2) = ordered(e1, e2, order)
    in
      move(opnd1, opndRd);
      case opnd2 
       of I.Immed _ => emit(I.BINARY{binOp=oper, src=opnd2, dst=opndRd})
        | _ => (move(opnd2, ecx);
		emit(I.BINARY{binOp=oper, src=ecx, dst=opndRd}))
      (*esac*);
      rd
    end (* shift *)

    (* Divisor must be in EDX:EAX *)
    fun divide(oper, signed, e1, e2, order) = let
      val (opnd1, opnd2) = ordered(e1, e2, order)
    in
      move(opnd1, eax);
      if signed then emit(I.CDQ) else move(I.Immed(0), edx);
      emit(I.MULTDIV{multDivOp=oper, src=regOrMem opnd2});
      move(eax, opndRd);
      rd
    end

    (* unsigned integer multiplication *)
    fun uMultiply(e1, e2) = 
      (* note e2 can never be (I.Direct edx) *)
      (move(operand e1, eax);
       emit(I.MULTDIV{multDivOp=I.UMUL, src=regOrMem(operand e2)});
       move(eax, opndRd);
       rd)

    (* signed integer multiplication *)
      
    (* The only forms that are allowed that also sets the 
     * OF and CF flags are:
     *
     *      imul r32, r32/m32, imm8
     *	    imul r32, imm8
     *      imul r32, imm32
     *)
    fun multiply(e1, e2) = let
      fun doit(i1 as I.Immed _, i2 as I.Immed _) =
 	   (move(i1, opndRd);
	    emit(I.MUL3{dst=rd, src1=i2, src2=NONE}))
	| doit(rm, i2 as I.Immed _) = doit(i2, rm)
	| doit(imm as I.Immed(i), rm) = 
	     emit(I.MUL3{dst=rd, src1=rm, src2=SOME i})
	| doit(r1 as I.Direct _, r2 as I.Direct _) =
	    (move(r1, opndRd);
	     emit(I.MUL3{dst=rd, src1=r2, src2=NONE}))
	| doit(r1 as I.Direct _, rm) =
	    (move(r1, opndRd);
	     emit(I.MUL3{dst=rd, src1=rm, src2=NONE}))
	| doit(rm, r as I.Direct _) = doit(r, rm)
	| doit(rm1, rm2) = 
	   (move(rm1, opndRd);
	    emit(I.MUL3{dst=rd, src1=rm2, src2=NONE}))
    in doit(ordered(e1, e2, T.LR))
    end

    fun trap() = 
      (case !trapLabel
       of NONE => (trapLabel := SOME(Label.newLabel "trap"); trap())
        | SOME lab => emit(I.JCC{cond=I.O, opnd=I.ImmedLabel(LE.LABEL lab)})
      (*esac*))
  in
    case exp
     of T.REG rs => (move(I.Direct rs, opndRd); rd)
      | T.LI n	 => (move(I.Immed(toInt32 n), opndRd); rd)
      | T.LI32 w => (move(I.Immed(wToInt32 w), opndRd); rd)
      | T.CONST c => (move(I.Const c, opndRd); rd)
      | T.LABEL lab => (move(I.ImmedLabel lab, opndRd); rd)
      | T.ADD(e, T.LI 1) => unary(I.INC, e)
      | T.ADD(e, T.LI32 0w1) => unary(I.INC, e)
      | T.ADD(e, T.LI ~1) => unary(I.DEC, e)
      | T.ADD(e1, e2) => 
          ((emit(I.LEA{r32=rd, addr=ea(exp)}); rd) 
	    handle EA => binary(true, I.ADD, e1, e2, T.LR))
      | T.SUB(e, T.LI 1, _) => unary(I.DEC, e)
      | T.SUB(e, T.LI32 0w1, _)	=> unary(I.DEC, e)
      | T.SUB(e, T.LI ~1, _) => unary(I.INC, e)
      | T.SUB(e1, e2, ord) => binary(false, I.SUB, e1, e2, ord)
      | T.MULU(e1, e2) => uMultiply(e1, e2)
      | T.DIVU(e1, e2, ord) => (divide(I.UDIV, false, e1, e2, ord))
      | T.ADDT(e1, e2) => (binary(true,I.ADD,e1,e2,T.LR); trap(); rd)
      | T.MULT(e1, e2) => (multiply(e1, e2); trap(); rd)
      | T.SUBT(e1, e2, ord) => 
         (binary(false,I.SUB,e1,e2,ord); trap(); rd)
      | T.DIVT(e1, e2, ord) => 
	  (divide(I.IDIV, true, e1, e2, ord); trap(); rd)
      | T.LOAD32(exp, _) => (move(ea exp, opndRd); rd)
      | T.LOAD8(exp, _) => 
	  (emit(I.MOVE{mvOp=I.MOVZX, src=ea exp, dst=opndRd}); rd)
      | T.ANDB(e1, e2) => binary(true, I.AND, e1, e2, T.LR)
      | T.ORB(e1, e2) => binary(true, I.OR, e1, e2, T.LR)
      | T.XORB(e1, e2) => binary(true, I.XOR, e1, e2, T.LR)
      | T.SRA(e1, e2, ord) => shift(I.SAR, e1, e2, ord)
      | T.SRL(e1, e2, ord) => shift(I.SHR, e1, e2, ord)
      | T.SLL(e1, e2, ord) => shift(I.SHL, e1, e2, ord)
      | T.SEQ(stm, rexp)  => (reduceStm stm; reduceRegRd(rexp, rd))
  end (* reduceRegRd *)

  and reduceFexp(fexp) = let
    val ST = I.FDirect 0
    val ST1 = I.FDirect 1

    datatype su_numbers = 
       LEAF of int 
     | BINARY of int * su_numbers * su_numbers
     | UNARY of int * su_numbers

    fun label(LEAF n) = n
      | label(BINARY(n, _, _)) = n
      | label(UNARY(n, _)) = n

    datatype direction = LEFT | RIGHT

   (* Generate tree of sethi-ullman numbers *)
    fun suBinary(t1, t2) = let
      val su1 = suNumbering(t1, LEFT)
      val su2 = suNumbering(t2, RIGHT)
      val n1 = label su1
      val n2 = label su2
    in BINARY(if n1=n2 then n1+1 else Int.max(n1, n2), su1, su2)
    end
    
    and suUnary(t) = let
      val su = suNumbering(t, LEFT)
    in UNARY(label su, su)
    end

    and suNumbering(T.FREG _, LEFT) = LEAF 1
      | suNumbering(T.FREG _, RIGHT) = LEAF 0
      | suNumbering(T.LOADD _, LEFT) = LEAF 1
      | suNumbering(T.LOADD _, RIGHT) = LEAF 0
      | suNumbering(T.FADDD(t1, t2), _) = suBinary(t1, t2)
      | suNumbering(T.FMULD(t1, t2), _) = suBinary(t1, t2)
      | suNumbering(T.FSUBD(t1, t2, _), _) = suBinary(t1, t2)
      | suNumbering(T.FDIVD(t1, t2, _), _) = suBinary(t1, t2)
      | suNumbering(T.FABSD t, _) = suUnary(t)
      | suNumbering(T.FNEGD t, _) = suUnary(t)
      | suNumbering(T.CVTI2D t, _) = UNARY(1, LEAF 0)

    fun leafEA(T.FREG f) = I.FDirect f
      | leafEA(T.LOADD(t, _)) = ea t
      | leafEA _ = error "leafEA"

    fun cvti2d(t) = let
      val opnd = operand t
      fun doMemOpnd () =
	(emit(I.MOVE{mvOp=I.MOVL, src=opnd, dst=tempMem});
	 emit(I.FILD tempMem))
    in
      case opnd
      of I.Direct _ => doMemOpnd()
       | I.Immed _ => doMemOpnd()
       | _ => emit(I.FILD opnd)
    end

    (* traverse expression and su-number tree *)
    fun gencode(_, LEAF 0) = ()
      | gencode(f, LEAF 1) = emit(I.FLD(leafEA f))
      | gencode(t, BINARY(_, su1, LEAF 0)) = let
	  fun doit(oper, t1, t2) = 
	    (gencode(t1, su1); 
	     emit(I.FBINARY{binOp=oper, src=leafEA t2, dst=ST}))
	in
	  case t
	  of T.FADDD(t1, t2) => doit(I.FADD, t1, t2)
	   | T.FMULD(t1, t2) => doit(I.FMUL, t1, t2)
	   | T.FSUBD(t1, t2, _) => doit(I.FSUB, t1, t2)
	   | T.FDIVD(t1, t2, _) => doit(I.FDIV, t1, t2)
	end
      | gencode(fexp, BINARY(_, su1, su2)) = let
	  fun doit(t1, t2, oper, operP, operRP) = let
	   (* oper[P] =>  ST(1) := ST oper ST(1); [pop] 
	    * operR[P] => ST(1) := ST(1) oper ST; [pop]
	    *)
	    val n1 = label su1
	    val n2 = label su2
	  in
	    if n1 < n2 andalso n1 <= 7 then 
	      (gencode(t2, su2); 
	       gencode(t1, su1); 
	       emit(I.FBINARY{binOp=operP, src=ST, dst=ST1}))
	    else if n2 <= n1 andalso n2 <= 7 then
	      (gencode(t1, su1); 
	       gencode(t2, su2); 
	       emit(I.FBINARY{binOp=operRP, src=ST, dst=ST1}))
	    else let (* both labels > 7 *)
	        val fs = I.FDirect(newFreg())
	      in
	        gencode (t2, su2);
	        emit(I.FSTP fs);
	        gencode (t1, su1);
	        emit(I.FBINARY{binOp=oper, src=fs, dst=ST})
	      end
	  end
	in
	  case fexp
	  of T.FADDD(t1, t2) => doit(t1, t2, I.FADD, I.FADDP, I.FADDP)
	   | T.FMULD(t1, t2) => doit(t1, t2, I.FMUL, I.FMULP, I.FMULP)
	   | T.FSUBD(t1, t2, _) => doit(t1, t2, I.FSUB, I.FSUBP, I.FSUBRP)
	   | T.FDIVD(t1, t2, _) => doit(t1, t2, I.FDIV, I.FDIVP, I.FDIVRP)
	end
      | gencode(fexp, UNARY(_, LEAF 0)) = 
	(case fexp
	  of T.FABSD t => (emit(I.FLD(leafEA t)); emit(I.FUNARY(I.FABS)))
	   | T.FNEGD t => (emit(I.FLD(leafEA t)); emit(I.FUNARY(I.FCHS)))
	   | T.CVTI2D t => cvti2d(t)
	 (*esac*))
      | gencode(fexp, UNARY(_, su)) = let
	  fun doit(oper, t) = (gencode(t, su); emit(I.FUNARY(oper)))
	in
	  case fexp
	   of T.FABSD t => doit(I.FABS, t)
	    | T.FNEGD t => doit(I.FCHS, t)
	    | T.CVTI2D _ => error "gencode:UNARY:cvti2d"
        end

    val labels = suNumbering(fexp, LEFT)
  in gencode(fexp, labels)
  end (*reduceFexp*)
 
  fun mltreeComp mltree = let
    fun mltc(T.PSEUDO_OP pOp)     = F.pseudoOp pOp
      | mltc(T.DEFINELABEL lab)   = F.defineLabel lab
      | mltc(T.ENTRYLABEL lab)    = F.entryLabel lab
      | mltc(T.ORDERED mlts)      = F.ordered mlts
      | mltc(T.BEGINCLUSTER)      = (F.beginCluster(); trapLabel := NONE)
      | mltc(T.CODE stms)         = app reduceStm stms 
      | mltc(T.BLOCK_NAME name)   = F.blockName name
      | mltc(T.ENDCLUSTER regmap) = 
         (case !trapLabel
	  of NONE => ()
	   | SOME lab => (F.defineLabel lab; emit(I.INTO))
	  (*esac*);
	  F.endCluster regmap)
      | mltc(T.ESCAPEBLOCK regs)  = F.exitBlock regs
  in mltc mltree
  end 

  val mlriscComp  = reduceStm
end
