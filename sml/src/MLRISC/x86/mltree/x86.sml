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
       and type cond = MLTreeBasis.cond
       and type fcond = MLTreeBasis.fcond   
   val tempMem : X86Instr.operand) : MLTREECOMP = 
struct
  structure T = X86MLTree
  structure S = T.Stream
  structure I = X86Instr
  structure C = X86Cells

  structure W32 = Word32
  structure LE = LabelExp

  fun error msg = MLRiscErrorMsg.error("X86",msg)

  (* label where a trap is generated -- one per cluster *)
  val trapLabel = ref (NONE: Label.label option)

  fun selectInstructions 
       (S.STREAM{emit,defineLabel,entryLabel,blockName,pseudoOp,annotation,
                 beginCluster,endCluster,exitBlock,alias,phi,comment,...}) =
  let

  val newReg  = C.newReg
  val newFreg = C.newFreg

  (* annotations *) 
  fun mark'(i,[]) = i 
    | mark'(i,a::an) = mark'(I.ANNOTATION{i=i,a=a},an) 

  fun mark(i,an) = emit(mark'(i,an))

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

  fun move(src as I.Direct s, dst as I.Direct d, an) = 
      if s=d then () 
      else mark(I.COPY{dst=[d], src=[s], tmp=NONE}, an)
    | move(src, dst, an) = mark(I.MOVE{mvOp=I.MOVL, src=src, dst=dst}, an)

  fun moveToReg(opnd) = 
  let val dst = I.Direct(newReg())
  in  move(opnd, dst, []); dst
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


fun rexp(T.REG(_,r)) = ["r" ^ Int.toString r]
  | rexp(T.LI i)  = ["LI"]
  | rexp(T.LI32 i32) = ["LI32"]
  | rexp(T.LABEL le) = ["LABEL"]
  | rexp(T.CONST c) = ["CONST"]

  | rexp(T.ADD  (_, e1, e2)) = ["ADD("] @ rexp e1 @ (","::rexp e2) @ [")"]
  | rexp(T.SUB  (_, e1, e2)) = ["SUB"]
  | rexp(T.MULU (_, e1, e2)) = ["MULU"]
  | rexp(T.DIVU (_, e1, e2)) =  ["DIVU"]

  | rexp(T.ADDT (_, e1, e2)) =   ["ADDT"]
  | rexp(T.MULT (_, e1, e2)) =  ["MULT"]
  | rexp(T.SUBT (_, e1, e2)) = ["SUBT"]
  | rexp(T.DIVT (_, e1, e2)) = ["DIVT"]

  | rexp(T.LOAD (8, e, _)) = ["LOAD8("] @ rexp e @ [")"]
  | rexp(T.LOAD (32, e, _)) = ["LOAD32"]

  | rexp(T.ANDB (_, e1, e2)) =  ["AND"]
  | rexp(T.ORB  (_, e1, e2)) = ["OR"]
  | rexp(T.XORB (_, e1, e2)) = ["XOR"]

  | rexp(T.SRA  (_, e1, e2)) = ["SRA("] @ rexp e1 @ (","::rexp e2) @ [")"]
  | rexp(T.SRL  (_, e1, e2)) = ["SRL"]
  | rexp(T.SLL  (_, e1, e2)) = ["SLL"]

  | rexp(T.SEQ(s, e)) = ["SEQ("] @ stm s @ ("," :: rexp e) @ [")"]

and stm s = 
 (case s 
  of T.MV(_, r, e) => ["MV(", Int.toString r] @ (",":: rexp e) @ [")"]
   | T.FMV _ => ["FMV"] 
   | T.COPY _  => ["COPY"]
   | T.FCOPY _ => ["FCOPY"]
   | T.JMP _ => ["JMP"]
   | T.CALL _ => ["CALL"]
   | T.RET  => ["RET"]
   | T.STORE _ => ["STORE"]
   | T.FSTORE _ => ["FSTORE"]
   | T.BCC    _ => ["BCC"]
   | T.FBCC   _ => ["FBCC"]
  (*esac*))

fun prMLRisc s = print(concat(stm s))



  exception EA

  (* return an index computation *)
  fun index(arg as (T.SLL(_, t, T.LI n))) =
      if n > 0 andalso n <= 3 then {index=reduceReg t, scale=n}
      else {index=reduceReg arg, scale=0}
    | index t = {index=reduceReg t, scale=0}

  (* return effective address *)
  and ea(eatree,mem) = 
  let
    (* Need to ensure that the scale register is never %esp *)
    fun doImmed(n, I.Immed(k)) = (I.Immed(k+toInt32 n) 
                                     handle Overflow => raise EA)
      | doImmed(n, I.ImmedLabel le) = I.ImmedLabel(LE.PLUS(le, LE.CONST n))
      | doImmed(n, I.Const c) = 
          I.Displace{base=reduceReg(T.CONST c),disp=I.Immed(toInt32 n),mem=mem}

    fun doConst(c, I.Immed(0)) = I.Const c
      | doConst(c, d) = I.Displace{base=reduceReg(T.CONST c), disp=d, mem=mem} 

    fun doLabel(le, I.Immed(0)) = I.ImmedLabel le
      | doLabel(le, I.Immed(n)) = I.ImmedLabel(LE.PLUS(le, LE.CONST(Int32.toInt n)))
      | doLabel(le, I.Const c) = 
          I.Displace{base=reduceReg(T.CONST c), disp=I.ImmedLabel le,
                     mem=mem}

    fun newDisp(n, combine, I.Displace{base, disp, mem}) = 
          I.Displace{base=base, disp=combine(n, disp), mem=mem}
      | newDisp(n, combine, I.Indexed{base, index, scale, disp, mem}) = 
          I.Indexed{base=base, index=index, scale=scale, 
                    disp=combine(n, disp), mem=mem}
      | newDisp(n, combine, disp) = combine(n, disp)

    fun combineBase(tree, base) = 
      SOME(case base
           of NONE => reduceReg tree
            | SOME base => reduceReg(T.ADD(32, T.REG(32,base), tree))
           (*esac*))

    (* keep building a bigger and bigger effective address expressions *)
    fun doEA(T.LI n, mode) = newDisp(n, doImmed, mode)
      | doEA(T.LABEL le, mode) = newDisp(le, doLabel, mode)
      | doEA(T.CONST c, mode) = newDisp(c, doConst, mode)
      | doEA(t0 as T.SLL(_, t, T.LI scale), mode) = 
        if scale >= 1 andalso scale <= 3 then 
         (case mode
          of I.Displace{base, disp, mem} =>
               I.Indexed
                {base=SOME base, index=reduceReg t, scale=scale, 
                 disp=disp, mem=mem}
           | I.Indexed{base, index, scale, disp, mem} => 
              I.Indexed{base=combineBase(t0,base), 
                        index=index, scale=scale, disp=disp, mem=mem}
           | disp => 
              I.Indexed{base=NONE, index=reduceReg t, scale=scale, 
                        disp=disp, mem=mem}
         (*esac*))
        else
         (case mode
          of I.Displace{base, disp, mem} => 
               I.Displace{base=Option.valOf(combineBase(t0, SOME base)), 
                          disp=disp, mem=mem}
           | I.Indexed{base, index, scale, disp, mem} => 
               I.Indexed{base=combineBase(t0, base), 
                         index=index, scale=scale, disp=disp, mem=mem}
           | disp => I.Displace{base=reduceReg(t0), disp=disp, mem=mem}
         (*esac*))
      | doEA(T.ADD(_, t1, t2 as T.REG _), mode) = doEA(t1, doEA(t2, mode))
      | doEA(T.ADD(_, t1, t2), mode) = doEA(t2, doEA(t1, mode))
      | doEA(T.SUB(ty, t1, T.LI n), mode) = doEA(T.ADD(ty, t1, T.LI (~n)), mode)
      | doEA(t, I.Indexed{base, index, scale, disp, mem}) = 
          I.Indexed{base=combineBase(t, base), index=index, scale=scale,
                    disp=disp, mem=mem}
      | doEA(T.REG(_,r), I.Displace{base, disp, mem}) = 
          I.Indexed{base=SOME base, index=r, scale=0, disp=disp, mem=mem}
      | doEA(t, I.Displace{base, disp, mem}) =
          I.Indexed{base=SOME base, index=reduceReg t, scale=0,
                    disp=disp, mem=mem}
      | doEA(t, immed) = I.Displace{base=reduceReg t, disp=immed, mem=mem}
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
    | operand(T.REG(_,r)) = I.Direct r
    | operand(T.LOAD(32,t,mem)) = ea(t,mem)
    | operand(t) = I.Direct(reduceReg(t))

  (* operand with preferred target *)
  and operandRd(T.LI i, _) = I.Immed (toInt32 i)
    | operandRd(T.LI32 w, _) = I.Immed(wToInt32 w)
    | operandRd(T.REG(_,r), _)  = I.Direct r
    | operandRd(T.LOAD(32,t,mem), _) = ea(t,mem)
    | operandRd(t, rd) = I.Direct(reduceRegRd(t, rd, []))

  and cond T.LT = I.LT        | cond T.LTU = I.B
    | cond T.LE = I.LE        | cond T.LEU = I.BE
    | cond T.EQ = I.EQ        | cond T.NE = I.NE
    | cond T.GE = I.GE        | cond T.GEU = I.AE
    | cond T.GT = I.GT        | cond T.GTU = I.A

 (* reduce an MLRISC statement tree *)
  and reduceStm(T.MV(_, rd, exp),an) = 
      let fun mv src = mark(I.MOVE{mvOp=I.MOVL, src=src, dst=I.Direct rd},an)
      in  case operandRd(exp, rd)
          of opnd as I.Direct rd' => if rd'=rd then () else mv opnd
           | opnd => mv opnd
      end
    | reduceStm(T.FMV(_, fd, T.FREG(_,fs)),an) = 
       if fs=fd then () else mark(I.COPY{dst=[fd], src=[fs], tmp=NONE},an)
    | reduceStm(T.FMV(_, fd, T.FLOAD(_, t, mem)),an) =
       (mark(I.FLD(ea(t,mem)),an); emit(I.FSTP(I.FDirect fd)))
    | reduceStm(T.FMV(_, fd, e),an) = 
       (reduceFexp(e,an); emit(I.FSTP(I.FDirect fd)))
    | reduceStm(T.CCMV(0, exp),an) = reduceCC(exp, 0, an)
    | reduceStm(T.CCMV _,_) = error "reduceStm: CCMV"
    | reduceStm(T.COPY(_, dst as [_], src),an) = 
        mark(I.COPY{dst=dst, src=src, tmp=NONE},an)
    | reduceStm(T.COPY(_, dst, src),an) = 
        mark(I.COPY{dst=dst, src=src, tmp=SOME(I.Direct(newReg()))},an)
    | reduceStm(T.FCOPY(_, dst, src),an) = 
        mark(I.FCOPY{dst=dst, src=src, tmp=SOME(I.FDirect(newFreg()))},an)
    | reduceStm(T.JMP(T.LABEL lexp, labs),an) = 
        mark(I.JMP(I.ImmedLabel lexp, labs),an)
    | reduceStm(T.JMP(exp, labs),an) = mark(I.JMP (operand exp, labs),an)
    | reduceStm(T.CALL(t,def,use,mem),an) = 
      let val addCCreg = C.addCell C.CC
          fun addList([], acc) = acc
            | addList(T.GPR(T.REG(_,r))::regs, acc) = 
                 addList(regs, C.addReg(r, acc))
            | addList(T.FPR(T.FREG(_,r))::regs, acc) = 
                 addList(regs, C.addFreg(r, acc))
            | addList(T.CCR(T.CC cc)::regs, acc) = 
                 addList(regs, addCCreg(cc, acc))
            | addList(_::regs, acc) = addList(regs, acc)
      in  mark(I.CALL(operand t,
                      addList(def,C.empty),addList(use,C.empty),mem),an)
      end
    | reduceStm(T.RET,an) = mark(I.RET NONE,an)
    | reduceStm(T.STORE(8, t1, t2, mem),an) = 
      let val opnd = immedOrReg(operand t2)
          val src = 
            (case opnd
             of I.Direct r => 
                  if r = C.eax then opnd else (move(opnd,eax,[]); eax)
              | _ => opnd
             (*esac*))
      in  mark(I.MOVE{mvOp=I.MOVB, src=src, dst=ea(t1,mem)},an)
      end          
    | reduceStm(T.STORE(32, t1, t2, mem),an) = 
        move(immedOrReg(operand t2), ea(t1,mem), an)
    | reduceStm(T.FSTORE(64, t1, t2, mem),an) = 
       (case t2 
         of T.FREG(_,fs) => emit(I.FLD(I.FDirect fs))
          | e => reduceFexp(e,[])
        (*esac*);
        mark(I.FSTP(ea(t1,mem)),an))
    | reduceStm(T.BCC(_, T.CMP(ty, cc as (T.EQ | T.NE), t1, T.LI 0), 
                      lab), an) = 
      let val opnd1 = operand t1
          fun jcc() = mark(I.JCC{cond=cond cc, opnd=immedLabel lab},an)
      in  case t1
          of T.ANDB _ => jcc()
           | T.ORB _ =>  jcc()
           | T.XORB _ => jcc()
           | T.SRA _ =>  jcc()
           | T.SRL _ =>  jcc()
           | T.SLL _ =>  jcc()
           | _ => (emit(I.CMP{lsrc=opnd1, rsrc=I.Immed 0}); jcc())
      end                 
    | reduceStm(T.BCC(_, T.CMP(ty, cc, t1, t2), lab), an) = 
      let fun cmpAndBranch(cc, opnd1, opnd2) = 
            (emit(I.CMP{lsrc=opnd1, rsrc=opnd2});
             mark(I.JCC{cond=cond cc, opnd=immedLabel lab},an))

          val (opnd1, opnd2) = (operand t1, operand t2)
      in  if isImmediate opnd1 andalso isImmediate opnd2 then
            cmpAndBranch(cc, moveToReg opnd1, opnd2)
          else if isImmediate opnd1 then
            cmpAndBranch(MLTreeUtil.swapCond cc, opnd2, opnd1)
          else if isImmediate opnd2 then
            cmpAndBranch(cc, opnd1, opnd2)
          else case (opnd1, opnd2)
           of (_, I.Direct _) => cmpAndBranch(cc, opnd1, opnd2)
            | (I.Direct _, _) => cmpAndBranch(cc, opnd1, opnd2)
            | _ => cmpAndBranch(cc, moveToReg opnd1, opnd2)
           (*esac*)
      end
    | reduceStm(T.BCC(cc, T.CC(0), lab), an) = 
        mark(I.JCC{cond=cond cc, opnd=immedLabel lab},an)
    | reduceStm(T.BCC _,_) = error "reduceStm: BCC"
    | reduceStm(T.FBCC(_, T.FCMP(fty, fcc, t1, t2), lab),an) = 
      let fun compare() = 
          let fun ignoreOrder (T.FREG _) = true
                | ignoreOrder (T.FLOAD _) = true
                | ignoreOrder _ = false
              fun t2t1 () = (reduceFexp(t2,[]); reduceFexp(t1,[]))
          in  if ignoreOrder t1 orelse ignoreOrder t2 then t2t1()
              else (reduceFexp(t1,[]); reduceFexp(t2,[]); emit(I.FXCH))
              ;
              emit(I.FUCOMPP)
          end
          fun branch() = 
          let val eax = I.Direct C.eax
              fun andil i = emit(I.BINARY{binOp=I.AND,src=I.Immed(i),dst=eax})
              fun xoril i = emit(I.BINARY{binOp=I.XOR,src=I.Immed(i),dst=eax})
              fun cmpil i = emit(I.CMP{rsrc=I.Immed(i), lsrc=eax})
              fun j(cc, lab) = mark(I.JCC{cond=cc, opnd=immedLabel lab},an)
              fun sahf() = emit(I.SAHF)
          in  case fcc
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
      in  compare(); emit I.FNSTSW; branch() 
      end
    | reduceStm(T.FBCC _,_) = error "reduceStm: FBCC"
    | reduceStm(T.ANNOTATION(s,a),an) = reduceStm(s,a::an)

  and reduceCC(T.CMP(ty, _, t1, t2), 0, an) = 
      let val (opnd1, opnd2) = (operand t1, operand t2)
      in mark(I.CMP(
            case (opnd1, opnd2)
            of (I.Immed _, _) => {lsrc=moveToReg opnd1, rsrc=opnd2}
             | (I.ImmedLabel _, _) => {lsrc=moveToReg opnd1, rsrc=opnd2}
             | (I.Const _, _) => {lsrc=moveToReg opnd1, rsrc=opnd2}
             | (I.Direct _, _) => {lsrc=opnd1, rsrc=opnd2}
             | (_, I.Direct _) => {lsrc=opnd1, rsrc=opnd2}
             | _ => {lsrc=moveToReg opnd1, rsrc=opnd2}),an)
      end
    | reduceCC(T.CCMARK(e,a),rd,an) = reduceCC(e,rd,a::an)
    | reduceCC _ = error "reduceCC" 


  and reduceReg(T.REG(_,rd)) = rd
    | reduceReg(exp) = reduceRegRd(exp, newReg(), [])

 (* reduce to the register rd where possible.*)
  and reduceRegRd(exp, rd, an) = let
    val opndRd = I.Direct(rd)

    fun binary(comm, oper, e1, e2, an) = let
      fun emit2addr (opnd1, opnd2) = 
        (move(opnd1, opndRd, []);
         mark(I.BINARY{binOp=oper, dst=opndRd, src=opnd2},an);
         rd)
      fun commute(opnd1 as I.Immed _, opnd2) = (opnd2, opnd1)
        | commute(opnd1 as I.ImmedLabel _, opnd2) = (opnd2, opnd1)
        | commute(opnd1 as I.Const _, opnd2) = (opnd2, opnd1)
        | commute(opnd1, opnd2 as I.Direct _) = (opnd2, opnd1)
        | commute arg = arg

      val opnds = (operand e1, operand e2)
    in emit2addr(if comm then commute opnds else opnds)
    end (*binary*)

    fun unary(oper, exp, an) = 
      (move(operand exp, opndRd, []);
       mark(I.UNARY{unOp=oper, opnd=opndRd},an);
       rd)

   (* The shift count can be either an immediate or the ECX register *)
    fun shift(oper, e1, e2, an) = let
      val (opnd1, opnd2) = (operand e1, operand e2)
    in
      move(opnd1, opndRd, []);
      case opnd2 
       of I.Immed _ => mark(I.BINARY{binOp=oper, src=opnd2, dst=opndRd},an)
        | _ => (move(opnd2, ecx, []);
                mark(I.BINARY{binOp=oper, src=ecx, dst=opndRd},an))
      (*esac*);
      rd
    end (* shift *)

    (* Divisor must be in EDX:EAX *)
    fun divide(oper, signed, e1, e2, an) = 
    let val (opnd1, opnd2) = (operand e1, operand e2)
    in  move(opnd1, eax, []);
        if signed then emit(I.CDQ) else move(I.Immed(0), edx, []);
        mark(I.MULTDIV{multDivOp=oper, src=regOrMem opnd2},an);
        move(eax, opndRd, []);
        rd
    end

    (* unsigned integer multiplication *)
    fun uMultiply(e1, e2, an) = 
      (* note e2 can never be (I.Direct edx) *)
      (move(operand e1, eax, []);
       mark(I.MULTDIV{multDivOp=I.UMUL, src=regOrMem(operand e2)},an);
       move(eax, opndRd, []);
       rd)

    (* signed integer multiplication *)
      
    (* The only forms that are allowed that also sets the 
     * OF and CF flags are:
     *
     *      imul r32, r32/m32, imm8
     *            imul r32, imm8
     *      imul r32, imm32
     *)
    fun multiply(e1, e2, an) = let
      fun doit(i1 as I.Immed _, i2 as I.Immed _) =
            (move(i1, opndRd, []);
            mark(I.MUL3{dst=rd, src1=i2, src2=NONE},an))
        | doit(rm, i2 as I.Immed _) = doit(i2, rm)
        | doit(imm as I.Immed(i), rm) = 
             mark(I.MUL3{dst=rd, src1=rm, src2=SOME i},an)
        | doit(r1 as I.Direct _, r2 as I.Direct _) =
            (move(r1, opndRd, []);
             mark(I.MUL3{dst=rd, src1=r2, src2=NONE},an))
        | doit(r1 as I.Direct _, rm) =
            (move(r1, opndRd, []);
             mark(I.MUL3{dst=rd, src1=rm, src2=NONE},an))
        | doit(rm, r as I.Direct _) = doit(r, rm)
        | doit(rm1, rm2) = 
           (move(rm1, opndRd, []);
            mark(I.MUL3{dst=rd, src1=rm2, src2=NONE},an))
    in doit(operand e1, operand e2)
    end

    fun trap() = 
      (case !trapLabel
       of NONE => (trapLabel := SOME(Label.newLabel "trap"); trap())
        | SOME lab => emit(I.JCC{cond=I.O, opnd=I.ImmedLabel(LE.LABEL lab)})
      (*esac*))
  in
    case exp
     of T.REG(_,rs) => (move(I.Direct rs, opndRd, an); rd)
      | T.LI n         => (move(I.Immed(toInt32 n), opndRd, an); rd)
      | T.LI32 w => (move(I.Immed(wToInt32 w), opndRd, an); rd)
      | T.CONST c => (move(I.Const c, opndRd, an); rd)
      | T.LABEL lab => (move(I.ImmedLabel lab, opndRd, an); rd)
      | T.ADD(32, e, T.LI 1) => unary(I.INC, e, an)
      | T.ADD(32, e, T.LI32 0w1) => unary(I.INC, e, an)
      | T.ADD(32, e, T.LI ~1) => unary(I.DEC, e, an)
      | T.ADD(32, e1, e2) => 
          ((mark(I.LEA{r32=rd, addr=ea(exp,I.Region.readonly)}, an); rd) 
            handle EA => binary(true, I.ADD, e1, e2, an))
      | T.SUB(32, e, T.LI 1) => unary(I.DEC, e, an)
      | T.SUB(32, e, T.LI32 0w1)        => unary(I.DEC, e, an)
      | T.SUB(32, e, T.LI ~1) => unary(I.INC, e, an)
      | T.SUB(32, e1, e2) => binary(false, I.SUB, e1, e2, an)
      | T.MULU(32, e1, e2) => uMultiply(e1, e2, an)
      | T.DIVU(32, e1, e2) => (divide(I.UDIV, false, e1, e2, an))
      | T.ADDT(32, e1, e2) => (binary(true,I.ADD,e1,e2, an); trap(); rd)
      | T.MULT(32, e1, e2) => (multiply(e1, e2, an); trap(); rd)
      | T.SUBT(32, e1, e2) => 
         (binary(false,I.SUB,e1,e2, an); trap(); rd)
      | T.DIVT(32, e1, e2) => 
          (divide(I.IDIV, true, e1, e2, an); trap(); rd)
      | T.LOAD(32, exp, mem) => (move(ea(exp,mem), opndRd, an); rd)
      | T.LOAD(8, exp, mem) => 
          (mark(I.MOVE{mvOp=I.MOVZX, src=ea(exp,mem), dst=opndRd}, an); rd)
      | T.ANDB(32, e1, e2) => binary(true, I.AND, e1, e2, an)
      | T.ORB(32, e1, e2) => binary(true, I.OR, e1, e2, an)
      | T.XORB(32, e1, e2) => binary(true, I.XOR, e1, e2, an)
      | T.SRA(32, e1, e2) => shift(I.SAR, e1, e2, an)
      | T.SRL(32, e1, e2) => shift(I.SHR, e1, e2, an)
      | T.SLL(32, e1, e2) => shift(I.SHL, e1, e2, an)
      | T.SEQ(stm, rexp)  => (reduceStm(stm,[]); reduceRegRd(rexp, rd, an))
      | T.MARK(e,a) => reduceRegRd(e,rd,a::an)
  end (* reduceRegRd *)

  and reduceFexp(fexp, an) = let
    val ST = I.FDirect(C.FPReg 0)
    val ST1 = I.FDirect(C.FPReg 1)

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
      | suNumbering(T.FLOAD _, LEFT) = LEAF 1
      | suNumbering(T.FLOAD _, RIGHT) = LEAF 0
      | suNumbering(T.FADD(_, t1, t2), _) = suBinary(t1, t2)
      | suNumbering(T.FMUL(_, t1, t2), _) = suBinary(t1, t2)
      | suNumbering(T.FSUB(_, t1, t2), _) = suBinary(t1, t2)
      | suNumbering(T.FDIV(_, t1, t2), _) = suBinary(t1, t2)
      | suNumbering(T.FABS(_,t), _) = suUnary(t)
      | suNumbering(T.FNEG(_,t), _) = suUnary(t)
      | suNumbering(T.CVTI2F _, _) = UNARY(1, LEAF 0)
      | suNumbering(T.FMARK(e,a),x) = suNumbering(e,x)

    fun leafEA(T.FREG(_,f)) = I.FDirect f
      | leafEA(T.FLOAD(_, t, mem)) = ea(t,mem)
      | leafEA _ = error "leafEA"

    fun cvti2d(t,an) = let
      val opnd = operand t
      fun doMemOpnd () =
        (emit(I.MOVE{mvOp=I.MOVL, src=opnd, dst=tempMem});
         mark(I.FILD tempMem,an))
    in
      case opnd
      of I.Direct _ => doMemOpnd()
       | I.Immed _ => doMemOpnd()
       | _ => mark(I.FILD opnd, an)
    end

    (* traverse expression and su-number tree *)
    fun gencode(_, LEAF 0, an) = ()
      | gencode(T.FMARK(e,a), x, an) = gencode(e, x, a::an)
      | gencode(f, LEAF 1, an) = mark(I.FLD(leafEA f), an)
      | gencode(t, BINARY(_, su1, LEAF 0), an) = let
          fun doit(oper, t1, t2) = 
            (gencode(t1, su1, []); 
             mark(I.FBINARY{binOp=oper, src=leafEA t2, dst=ST},an))
        in
          case t
          of T.FADD(_, t1, t2) => doit(I.FADD, t1, t2)
           | T.FMUL(_, t1, t2) => doit(I.FMUL, t1, t2)
           | T.FSUB(_, t1, t2) => doit(I.FSUB, t1, t2)
           | T.FDIV(_, t1, t2) => doit(I.FDIV, t1, t2)
        end
      | gencode(fexp, BINARY(_, su1, su2), an) = let
          fun doit(t1, t2, oper, operP, operRP) = let
           (* oper[P] =>  ST(1) := ST oper ST(1); [pop] 
            * operR[P] => ST(1) := ST(1) oper ST; [pop]
            *)
            val n1 = label su1
            val n2 = label su2
          in
            if n1 < n2 andalso n1 <= 7 then 
              (gencode(t2, su2, []); 
               gencode(t1, su1, []); 
               mark(I.FBINARY{binOp=operP, src=ST, dst=ST1}, an))
            else if n2 <= n1 andalso n2 <= 7 then
              (gencode(t1, su1, []); 
               gencode(t2, su2, []); 
               mark(I.FBINARY{binOp=operRP, src=ST, dst=ST1}, an))
            else let (* both labels > 7 *)
                val fs = I.FDirect(newFreg())
              in
                gencode (t2, su2, []);
                emit(I.FSTP fs);
                gencode (t1, su1, []);
                mark(I.FBINARY{binOp=oper, src=fs, dst=ST}, an)
              end
          end
        in
          case fexp
          of T.FADD(_, t1, t2) => doit(t1, t2, I.FADD, I.FADDP, I.FADDP)
           | T.FMUL(_, t1, t2) => doit(t1, t2, I.FMUL, I.FMULP, I.FMULP)
           | T.FSUB(_, t1, t2) => doit(t1, t2, I.FSUB, I.FSUBP, I.FSUBRP)
           | T.FDIV(_, t1, t2) => doit(t1, t2, I.FDIV, I.FDIVP, I.FDIVRP)
        end
      | gencode(fexp, UNARY(_, LEAF 0), an) = 
        (case fexp
          of T.FABS(_, t) => (emit(I.FLD(leafEA t)); mark(I.FUNARY(I.FABS),an))
           | T.FNEG(_, t) => (emit(I.FLD(leafEA t)); mark(I.FUNARY(I.FCHS),an))
           | T.CVTI2F(_,_,t) => cvti2d(t,an)
         (*esac*))
      | gencode(fexp, UNARY(_, su), an) = let
          fun doit(oper, t) = (gencode(t, su, []); mark(I.FUNARY(oper),an))
        in
          case fexp
           of T.FABS(_, t) => doit(I.FABS, t)
            | T.FNEG(_, t) => doit(I.FCHS, t)
            | T.CVTI2F _ => error "gencode:UNARY:cvti2f"
        end

    val labels = suNumbering(fexp, LEFT)
  in gencode(fexp, labels, an)
  end (*reduceFexp*)
 
      fun doStm s = reduceStm(s,[])
      val beginCluster = fn _ => (trapLabel := NONE; beginCluster 0)
      val endCluster = fn a =>
         (case !trapLabel
          of NONE => ()
           | SOME lab => (defineLabel lab; emit(I.INTO))
          (*esac*);
          endCluster a)
  in S.STREAM
     {  beginCluster= beginCluster,
        endCluster  = endCluster,
        emit        = doStm,
        pseudoOp    = pseudoOp,
        defineLabel = defineLabel,
        entryLabel  = entryLabel,
        blockName   = blockName,
        comment     = comment,
        annotation  = annotation,
        exitBlock   = exitBlock,
        alias       = alias,
        phi         = phi
     }
  end 

end
