(* amd64.sml
 *
 * This file performs instruction selection for AMD64.  There are only
 * a few differences between this module and its x86 equivalent.  The
 * 64-bit ops are now supported, but this required overhauling
 * the 32-bit-defaulting x86 code.  In particular, a mix of 32- and
 * 64-bit instructions can be intermixed where they could not before.
 *)

local
   val enableFastFPMode = true (* set this to false to disable the mode *)
in

functor AMD64
  (structure AMD64Instr : AMD64INSTR
   structure MLTreeUtils : MLTREE_UTILS
			where T = AMD64Instr.T
   structure ExtensionComp : MLTREE_EXTENSION_COMP
     			where I = AMD64Instr and T = AMD64Instr.T
   structure MLTreeStream : MLTREE_STREAM
			where T = ExtensionComp.T
    val cvti2f : 
         {ty: AMD64Instr.T.ty, 
          src: AMD64Instr.operand, 
             (* source operand, guaranteed to be non-memory! *)
          an: Annotations.annotations ref (* cluster annotations *)
         } -> 
         {instrs : AMD64Instr.instruction list,(* the instructions *)
          tempMem: AMD64Instr.operand,         (* temporary for CVTI2F *)
          cleanup: AMD64Instr.instruction list (* cleanup code *)
         }
    (* When the following flag is set, we allocate floating point registers
     * directly on the floating point stack
     *)
    val fast_floating_point : bool ref 
    val defaultIntTy : AMD64Instr.T.ty
  ) :  MLTREECOMP 
       = 
struct

  structure I = AMD64Instr
  structure T = I.T
  structure TRS = MLTreeSize (structure T=T
                              val intTy = defaultIntTy)
  structure TS = ExtensionComp.TS
  structure C = I.C
  structure Shuffle = Shuffle(I)
  structure W32 = Word32
  structure A = MLRiscAnnotations
  structure CFG = ExtensionComp.CFG
  structure CB = CellsBasis

  type instrStream = (I.instruction,C.cellset,CFG.cfg) TS.stream
  type mltreeStream = (T.stm,T.mlrisc list,CFG.cfg) TS.stream

  datatype kind = REAL | INTEGER

  val defaultAddrTy = 64
 
  structure Gen = MLTreeGen
     (structure T = T
      structure Cells = C
      val intTy = defaultIntTy
      val naturalWidths = [32, 64]
      datatype rep = SE | ZE | NEITHER
      val rep = NEITHER
     )

  fun error msg = MLRiscErrorMsg.error("AMD64",msg)

  val opcodes8 = {INC=I.INCB,DEC=I.DECB,ADD=I.ADDB,SUB=I.SUBB,
                  NOT=I.NOTB,NEG=I.NEGB,
                  SHL=I.SHLB,SHR=I.SHRB,SAR=I.SARB,
                  OR=I.ORB,AND=I.ANDB,XOR=I.XORB,
		  MUL=I.MULB,IMUL=I.IMULB,
		  CMP=I.CMPB,MOV=I.MOVB}
  val opcodes16 = {INC=I.INCW,DEC=I.DECW,ADD=I.ADDW,SUB=I.SUBW,
                   NOT=I.NOTW,NEG=I.NEGW,
                   SHL=I.SHLW,SHR=I.SHRW,SAR=I.SARW,
                   OR=I.ORW,AND=I.ANDW,XOR=I.XORW,
		   MUL=I.MULW,IMUL=I.IMULW,
		   CMP=I.CMPW,MOV=I.MOVW}
  val opcodes32 = {INC=I.INCL,DEC=I.DECL,ADD=I.ADDL,SUB=I.SUBL,
                   NOT=I.NOTL,NEG=I.NEGL,
                   SHL=I.SHLL,SHR=I.SHRL,SAR=I.SARL,
                   OR=I.ORL,AND=I.ANDL,XOR=I.XORL,
		   MUL=I.MULL,IMUL=I.IMULL,
		   CMP=I.CMPL, MOV=I.MOVL}
  val opcodes64 = {INC=I.INCQ,DEC=I.DECQ,ADD=I.ADDQ,SUB=I.SUBQ,
                   NOT=I.NOTQ,NEG=I.NEGQ,
                   SHL=I.SHLQ,SHR=I.SHRQ,SAR=I.SARQ,
                   OR=I.ORQ,AND=I.ANDQ,XOR=I.XORQ,
		   MUL=I.MULQ,IMUL=I.IMULQ,
		   CMP=I.CMPQ, MOV=I.MOVQ}

  fun opC opc ty = 
      let val opcodes = (
	      case ty
	       of 8 => opcodes8
		| 16 => opcodes16
		| 32 => opcodes32
		| 64 => opcodes64
	  (* esac *))
      in opc opcodes end (* opC *)

  val notOp = opC #NOT
  val incOp = opC #INC
  val decOp = opC #DEC
  val addOp = opC #ADD
  val subOp = opC #SUB
  val notOp = opC #NOT
  val negOp = opC #NEG
  val shlOp = opC #SHL
  val shrOp = opC #SHR
  val sarOp = opC #SAR
  val orOp  = opC #OR
  val andOp = opC #AND
  val xorOp = opC #XOR
  val movOp = opC #MOV
  val cmpOp = opC #CMP
  val mulOp = opC #MUL
  val imulOp = opC #IMUL
  fun div1Op 32 = I.DIVL1
    | div1Op 64 = I.DIVQ1
  fun idiv1Op 32 = I.IDIVL1
    | idiv1Op 64 = I.IDIVQ1
  fun mul1Op 32 = I.MULL1
    | mul1Op 64 = I.MULQ1
  fun imul1Op 32 = I.IMULL1
    | imul1Op 64 = I.IMULQ1
  val divOp = div1Op
  val idivOp = idiv1Op

  (* The following hardcoded *)
  fun isFMemReg r = if enableFastFPMode andalso !fast_floating_point
                    then let val r = CB.registerNum r
                         in r >= 8 andalso r < 32 end
                    else true
  val isAnyFMemReg = List.exists (fn r => 
                                  let val r = CB.registerNum r  
                                  in  r >= 8 andalso r < 32 end
                                 )

  val ST0 = C.ST 0
  val ST7 = C.ST 7

  fun fsize 32 = I.FP32
    | fsize 64 = I.FP64
    | fsize 80 = I.FP80
    | fsize _  = error "fsize"

  (* flag floating point generation *)
  val floatingPointUsed = ref false
			  
  (* effective address of an integer register *)
  fun IntReg ty r = I.Direct (ty,r)
  and RealReg r = if isFMemReg r then I.FDirect r else I.FPR r

  (* label where a trap is generated -- one per cluster *)
  val trapLabel = ref (NONE: (I.instruction * Label.label) option)

  val newReg  = C.newReg
  val newFreg = C.newFreg

  (* some useful registers *)
  fun rax ty = I.Direct(ty,C.rax)
  fun rcx ty = I.Direct(ty,C.rcx)
  fun rdx ty = I.Direct(ty,C.rdx)

  (* convert mlrisc to cellset: *)
  fun cellset mlrisc =
      let val addCCReg = CB.CellSet.add 
          fun g([],acc) = acc
            | g(T.GPR(T.REG(_,r))::regs,acc)  = g(regs,C.addReg(r,acc))
            | g(T.FPR(T.FREG(_,f))::regs,acc) = g(regs,C.addFreg(f,acc))
            | g(T.CCR(T.CC(_,cc))::regs,acc)  = g(regs,addCCReg(cc,acc))
            | g(T.CCR(T.FCC(_,cc))::regs,acc)  = g(regs,addCCReg(cc,acc))
            | g(_::regs, acc) = g(regs, acc)
      in  g(mlrisc, C.empty) end


  (* conversions *)
  val itow = Word.fromInt
  val wtoi = Word.toInt
  fun toInt32 i = T.I.toInt32(32, i)
  val w32toi32 = Word32.toLargeIntX 
  val i32tow32 = Word32.fromLargeInt		 
  (* One day, this is going to bite us when precision(LargeInt)>32 *)
  fun wToInt32 w = Int32.fromLarge(Word32.toLargeIntX w)

  (* analyze for power-of-two-ness *)
  fun analyze i' = 
      let val i = toInt32 i'
      in
	  let val (isneg, a, w) =
		  if i >= 0 then (false, i, T.I.toWord32 (32, i'))
		  else (true, ~i, T.I.toWord32 (32, T.I.NEG (32,  i')))
	      fun log2 (0w1, p) = p
		| log2 (w, p) = log2 (W32.>> (w, 0w1), p + 1)
	  in
	      if w > 0w1 andalso W32.andb (w - 0w1, w) = 0w0 then
		  (i, SOME (isneg, a,
			    T.LI (T.I.fromInt32 (32, log2 (w, 0)))))
	      else (i, NONE)
	  end handle _ => (i, NONE)
      end


  val readonly = I.Region.readonly

  fun immedLabel lab = I.ImmedLabel(T.LABEL lab)
		       
  (* Is the expression zero? *)
  fun isZero(T.LI z) = z = 0
    | isZero(T.MARK(e,a)) = isZero e
    | isZero _ = false
  (* Does the expression set the zero bit? 
   * WARNING: we assume these things are not optimized out!
   *)
  fun setZeroBit(T.ANDB _)     = true
    | setZeroBit(T.ORB _)      = true
    | setZeroBit(T.XORB _)     = true
    | setZeroBit(T.SRA _)      = true
    | setZeroBit(T.SRL _)      = true
    | setZeroBit(T.SLL _)      = true
    | setZeroBit(T.SUB _)      = true
    | setZeroBit(T.ADDT _)     = true
    | setZeroBit(T.SUBT _)     = true
    | setZeroBit(T.MARK(e, _)) = setZeroBit e
    | setZeroBit _             = false
				 
  fun setZeroBit2(T.ANDB _)     = true
    | setZeroBit2(T.ORB _)      = true
    | setZeroBit2(T.XORB _)     = true
    | setZeroBit2(T.SRA _)      = true
    | setZeroBit2(T.SRL _)      = true
    | setZeroBit2(T.SLL _)      = true
    | setZeroBit2(T.ADD(_, _, _)) = true (* can't use leal! *)
    | setZeroBit2(T.SUB _)      = true
    | setZeroBit2(T.ADDT _)     = true
    | setZeroBit2(T.SUBT _)     = true
    | setZeroBit2(T.MARK(e, _)) = setZeroBit2 e
    | setZeroBit2 _             = false

  fun selectInstructions 
       (instrStream as
        TS.S.STREAM{emit=emitInstruction, defineLabel, entryLabel, pseudoOp,
		    annotation, getAnnotations, beginCluster, endCluster, exitBlock,
		    comment, ...}) =
      let val emit = emitInstruction o I.INSTR
	  val emits = app emitInstruction
	  exception EA

	  (* mark an expression with a list of annotations *) 
	  fun mark'(i,[]) = emitInstruction(i)
            | mark'(i,a::an) = mark'(I.ANNOTATION{i=i,a=a},an) 
	  (* annotate an expression and emit it *)
	  fun mark(i,an) = mark'(I.INSTR i,an)
	       
	  (* Add an overflow trap *)
	  fun trap() =
	      let val jmp = 
		      case !trapLabel of 
			  NONE => let val label = Label.label "trap" ()
				     val jmp   = 
					 I.ANNOTATION{i=I.jcc{cond=I.O, 
							      opnd=I.ImmedLabel(T.LABEL label)},
						      a=MLRiscAnnotations.BRANCHPROB (Probability.unlikely)}
				 in  trapLabel := SOME(jmp, label); jmp end
			| SOME(jmp, _) => jmp
	      in emitInstruction jmp end

      (* emit parallel copies for integers *)
      fun copy(ty, [], [], an) = ()
        | copy(ty, dst, src, an) = 
          let fun mvInstr{dst=I.Direct (_,rd), src=I.Direct (_,rs)} = 
                    if CB.sameColor(rd,rs) then [] 
                    else [I.COPY{k=CB.GP, sz=ty, dst=[rd], src=[rs], tmp=NONE}]
                | mvInstr{dst, src} = [I.move{mvOp=movOp ty, src=src, dst=dst}]
          in
             emits (Shuffle.shuffle {mvInstr=mvInstr, ea=IntReg ty}
               {tmp=SOME(I.Direct(ty,newReg())),
                dst=dst, src=src})
          end

      fun zero (ty, dst) = emit(I.BINARY{binOp=xorOp ty, src=dst, dst=dst})
      (* move and annotate *)
      fun move' (ty, src as I.Direct (_, s), dst as I.Direct (_, d), an) =
	  if CB.sameColor (s, d) then ()
	  else mark' (I.COPY{k=CB.GP, sz=ty, dst=[d], src=[s], tmp=NONE}, an)
        | move'(ty, I.Immed 0, dst as I.Direct (_,d), an) = 
            mark(I.BINARY{binOp=xorOp ty, src=dst, dst=dst}, an)
        | move'(ty, src, dst, an) = mark(I.MOVE{mvOp=movOp ty, src=src, dst=dst}, an)

      (* Move only! *)  
      fun move(ty, src, dst) = move'(ty, src, dst, [])

      (* Translates MLTREE condition code to amd64 condition code *)
      fun cond T.LT = I.LT | cond T.LTU = I.B
        | cond T.LE = I.LE | cond T.LEU = I.BE
        | cond T.EQ = I.EQ | cond T.NE  = I.NE
        | cond T.GE = I.GE | cond T.GEU = I.AE
        | cond T.GT = I.GT | cond T.GTU = I.A
	| cond cc = error(concat["cond(", T.Basis.condToString cc, ")"])

      (* emit parallel copies for floating point 
       * Normal version.
       *)
      fun fcopy'(fty, [], [], _) = ()
        | fcopy'(fty, dst as [_], src as [_], an) = 
            mark'(I.COPY{k=CB.FP, sz=fty, dst=dst,src=src,tmp=NONE}, an)
        | fcopy'(fty, dst, src, an) = 
            mark'(I.COPY{k=CB.FP, sz=fty, dst=dst,src=src,tmp=SOME(I.FDirect(newFreg()))}, an)

      (* emit parallel copies for floating point.
       * Fast version.
       * Translates parallel copies that involve memregs into 
       * individual copies.
       *)
       
      fun fcopy''(fty, [], [], _) = ()
        | fcopy''(fty, dst, src, an) = 
          if true orelse isAnyFMemReg dst orelse isAnyFMemReg src then
          let val fsize = fsize fty
              fun mvInstr{dst, src} = [I.fmove{fsize=fsize, src=src, dst=dst}]
          in
              emits (Shuffle.shuffle{mvInstr=mvInstr, ea=RealReg}
                {tmp=case dst of
                       [_] => NONE
                     |  _  => SOME(I.FPR(newReg())),
                 dst=dst, src=src})
          end
          else
            mark'(I.COPY{k=CB.FP, sz=fty, dst=dst,
			src=src,tmp=
                         case dst of
                           [_] => NONE
                         | _   => SOME(I.FPR(newFreg()))}, an)
 
      fun fcopy x = if enableFastFPMode andalso !fast_floating_point 
                    then fcopy'' x else fcopy' x

      fun address' ty (ea, mem) =
	  let fun makeAddressingMode(NONE, NONE, _, disp) = disp
		| makeAddressingMode(SOME base, NONE, _, disp) = 
                  I.Displace{base=base, disp=disp, mem=mem}
		| makeAddressingMode(base, SOME index, scale, disp) = 
                  I.Indexed{base=base, index=index, scale=scale,
                            disp=disp, mem=mem}

	      (* Keep building a bigger and bigger effective address expressions 
	       * The input is a list of trees
	       * b -- base
	       * i -- index
	       * s -- scale
	       * d -- immed displacement
	       *)	      
	      fun doEA([], b, i, s, d) = makeAddressingMode(b, i, s, d)
		| doEA(t::trees, b, i, s, d) =
		  (case t of 
                       T.LI n   => doEAImmed(trees, toInt32 n, b, i, s, d)
		     | T.CONST _ => doEALabel(trees, t, b, i, s, d)
		     | T.LABEL _ => doEALabel(trees, t, b, i, s, d)
		     | T.LABEXP le => doEALabel(trees, le, b, i, s, d)
		     | T.ADD(ty, t1, t2 as T.REG(_,r)) => 
		       doEA(t1::t2::trees, b, i, s, d)
		     | T.ADD(ty, t1, t2) => doEA(t1::t2::trees, b, i, s, d)
		     | T.SUB(ty, t1, T.LI n) => 
		       doEA(t1::T.LI(T.I.NEG(ty,n))::trees, b, i, s, d)
		     | T.SLL(ty, t1, T.LI n) => let
			   val n = T.I.toInt(ty, n)
                       in 
			   case n
			    of 0 => displace(trees, t1, b, i, s, d)
	  		     | 1 => indexed(trees, t1, t, 1, b, i, s, d)
	  		     | 2 => indexed(trees, t1, t, 2, b, i, s, d)
			     | 3 => indexed(trees, t1, t, 3, b, i, s, d)
			     | _ => displace(trees, t, b, i, s, d)
                       end
		     | t => displace(trees, t, b, i, s, d)
		  (* esac *))

              (* Add an immed constant *)
              and doEAImmed(trees, 0, b, i, s, d) = doEA(trees, b, i, s, d)
		| doEAImmed(trees, n, b, i, s, I.Immed m) = 
                  doEA(trees, b, i, s, I.Immed(n+m))
		| doEAImmed(trees, n, b, i, s, I.ImmedLabel le) = 
                  doEA(trees, b, i, s, 
                       I.ImmedLabel(T.ADD(ty,le,T.LI(T.I.fromInt32(ty, n)))))
		| doEAImmed(trees, n, b, i, s, _) = error "doEAImmed"
						    
              (* Add a label expression *)
              and doEALabel(trees, le, b, i, s, I.Immed 0) = 
                  doEA(trees, b, i, s, I.ImmedLabel le)
		| doEALabel(trees, le, b, i, s, I.Immed m) = 
                  doEA(trees, b, i, s, 
                       I.ImmedLabel(T.ADD(ty,le,T.LI(T.I.fromInt32(ty, m))))
                       handle Overflow => error "doEALabel: constant too large")
		| doEALabel(trees, le, b, i, s, I.ImmedLabel le') = 
                  doEA(trees, b, i, s, I.ImmedLabel(T.ADD(ty,le,le')))
		| doEALabel(trees, le, b, i, s, _) = error "doEALabel"
						     
              (* generate code for tree and ensure that it is not in %rsp *)
              and exprNotRsp tree =
		  let val r = expr tree
		  in  if CB.sameColor(r, C.rsp) then 
			  let val tmp = newReg()
			  in  move(ty, I.Direct (ty,r), I.Direct (ty,tmp)); tmp end
                      else r
		  end

              (* Add a base register *)
              and displace(trees, t, NONE, i, s, d) =  (* no base yet *)
		  doEA(trees, SOME(expr t), i, s, d)
		| displace(trees, t, b as SOME base, NONE, _, d) = (* no index *)
		  (* make t the index, but make sure that it is not %rsp! *)
		  let val i = expr t
		  in  if CB.sameColor(i, C.rsp) then
			  (* swap base and index *)
			  if CB.sameColor(base, C.rsp) then
			      doEA(trees, SOME i, b, 0, d)
			  else  (* base and index = %rsp! *)
			      let val index = newReg()
			      in  move(ty, I.Direct (ty,i), I.Direct (ty,index));
				  doEA(trees, b, SOME index, 0, d)
			      end
                      else
			  doEA(trees, b, SOME i, 0, d)
		  end
		| displace(trees, t, SOME base, i, s, d) = (* base and index *) 
		  let val b = expr(T.ADD(ty,T.REG(ty,base),t))
		  in  doEA(trees, SOME b, i, s, d) end
		  
              (* Add an indexed register *)
              and indexed(trees, t, t0, scale, b, NONE, _, d) = (* no index yet *)
		  doEA(trees, b, SOME(exprNotRsp t), scale, d)
		| indexed(trees, _, t0, _, NONE, i, s, d) = (* no base *)
		  doEA(trees, SOME(expr t0), i, s, d)
		| indexed(trees, _, t0, _, SOME base, i, s, d) = (*base and index*)
		  let val b = expr(T.ADD(ty, t0, T.REG(ty, base)))
		  in  doEA(trees, SOME b, i, s, d) end
	  in 
	      case doEA ([ea], NONE, NONE, 0, I.Immed 0)
	       of I.Immed _ => raise EA
		| I.ImmedLabel le => I.LabelEA le
		| ea => ea
	  end (* address' *)

      and address (ea, mem) = address' defaultAddrTy (ea, mem)

          (* reduce an expression into an operand *)
      and operand ty (T.LI i) = I.Immed(toInt32(i)) 
        | operand _ (x as (T.CONST _ | T.LABEL _)) = I.ImmedLabel x
        | operand _ (T.LABEXP le) = I.ImmedLabel le
        | operand _ (T.REG(ty,r)) = IntReg ty r
        | operand _ (T.LOAD(ty,ea,mem)) = address (ea, mem)
        | operand ty t = I.Direct(ty,expr t)

      and moveToReg(ty,opnd) =
          let val dst = I.Direct(ty,newReg())
          in  move(ty,opnd, dst); dst
          end

      and reduceOpnd(ty, I.Direct (_,r)) = r
        | reduceOpnd (ty, opnd) =
          let val dst = newReg()
          in move(ty, opnd, I.Direct (ty,dst)); dst end

      (* ensure that the operand is either an immed or register *)
      and immedOrReg(ty, opnd as I.Displace _) = moveToReg (ty, opnd)
        | immedOrReg(ty, opnd as I.Indexed _)  = moveToReg (ty, opnd)
(*        | immedOrReg(ty, opnd as I.MemReg _)   = moveToReg (ty, opnd)*)
        | immedOrReg(ty, opnd as I.LabelEA _)  = moveToReg (ty, opnd)
        | immedOrReg (ty, opnd)  = opnd

      and isImmediate(I.Immed _) = true
        | isImmediate(I.ImmedLabel _) = true
        | isImmediate _ = false

      and regOrMem (ty, opnd) = if isImmediate opnd then moveToReg (ty, opnd) else opnd
 
      and isMemOpnd opnd = 
          (case opnd of 
            I.Displace _ => true
          | I.Indexed _  => true 
          | I.LabelEA _  => true 
          | I.FDirect f  => true
          | _            => false
          )

      and genExpr exp = let val rd = newReg() in doExpr(exp, rd, []); rd end

      (* generate an expression and return its result register 
       * If rewritePseudo is on, the result is guaranteed to be in a 
       * non memReg register
       *)
      and expr(exp as T.REG(_, rd)) = rd
        | expr exp = genExpr exp
		     
      (*========================================================
       * Floating point code generation starts here.
       * Some generic fp routines first.
       *========================================================*)

       (* Can this tree be folded into the src operand of a floating point
        * operations?
        *)
      and foldableFexp(T.FREG _) = true
        | foldableFexp(T.FLOAD _) = true
        | foldableFexp(T.CVTI2F(_, (16 | 32), _)) = true
        | foldableFexp(T.CVTF2F(_, _, t)) = foldableFexp t
        | foldableFexp(T.FMARK(t, _)) = foldableFexp t
        | foldableFexp _ = false

        (* Move integer e of size ty into a memory location.
         * Returns a quadruple: 
         *  (INTEGER,return ty,effect address of memory location,cleanup code) 
         *) 
(* FIXME: handle the return size properly *)
      and convertIntToFloat(ty, e) = 
          let val opnd = operand ty e 
          in  if isMemOpnd opnd andalso (ty = 16 orelse ty = 32 orelse ty = 64)
              then (INTEGER, ty, opnd, [])
              else 
                let val {instrs, tempMem, cleanup} = 
                        cvti2f{ty=ty, src=opnd, an=getAnnotations()}
                in  emits instrs;
                    (INTEGER, 32, tempMem, cleanup)
                end
          end
 
      (*========================================================
       * Sethi-Ullman based floating point code generation as 
       * implemented by Lal 
       *========================================================*)

      and fld(32, opnd) = I.FLDS opnd
        | fld(64, opnd) = I.FLDL opnd
        | fld(80, opnd) = I.FLDT opnd
        | fld _         = error "fld"

      and fild(16, opnd) = I.FILD opnd
        | fild(32, opnd) = I.FILDL opnd
        | fild(64, opnd) = I.FILDLL opnd
        | fild _         = error "fild"

      and fxld(INTEGER, ty, opnd) = fild(ty, opnd)
        | fxld(REAL, fty, opnd) = fld(fty, opnd)

      and fstp(32, opnd) = I.FSTPS opnd
        | fstp(64, opnd) = I.FSTPL opnd
        | fstp(80, opnd) = I.FSTPT opnd
        | fstp _         = error "fstp"

          (* generate code for floating point stores *)
      and fstore'(fty, ea, d, mem, an) = 
          (case d of
             T.FREG(fty, fs) => emit(fld(fty, I.FDirect fs))
           | _ => reduceFexp(fty, d, []);
           mark(fstp(fty, address(ea, mem)), an)
          )

          (* generate code for floating point loads *)
      and fload'(fty, ea, mem, fd, an) = 
            let val ea = address(ea, mem)
            in  mark(fld(fty, ea), an); 
                if CB.sameColor(fd,ST0) then () 
                else emit(fstp(fty, I.FDirect fd))
            end

      and fexpr' e = (reduceFexp(64, e, []); C.ST(0))

          (* 
           * Generate floating point expression using Sethi-Ullman's scheme:
           * This function evaluates a floating point expression, 
           * and put result in %ST(0).
           *)
      and reduceFexp(fty, fexp, an)  = 
          let val ST = I.ST(C.ST 0)
              val ST1 = I.ST(C.ST 1)
              val cleanupCode = ref [] : I.instruction list ref

              datatype su_tree = 
                LEAF of int * T.fexp * ans
              | BINARY of int * T.fty * fbinop * su_tree * su_tree * ans
              | UNARY of int * T.fty * I.funOp * su_tree * ans
              and fbinop = FADD | FSUB | FMUL | FDIV
                         | FIADD | FISUB | FIMUL | FIDIV
              withtype ans = Annotations.annotations
 
              fun label(LEAF(n, _, _)) = n
                | label(BINARY(n, _, _, _, _, _)) = n
                | label(UNARY(n, _, _, _, _)) = n

              fun annotate(LEAF(n, x, an), a)  = LEAF(n,x,a::an)
                | annotate(BINARY(n,t,b,x,y,an), a) = BINARY(n,t,b,x,y,a::an)
                | annotate(UNARY(n,t,u,x,an), a) = UNARY(n,t,u,x,a::an)

              (* Generate expression tree with sethi-ullman numbers *)
              fun su(e as T.FREG _)       = LEAF(1, e, [])
                | su(e as T.FLOAD _)      = LEAF(1, e, [])
                | su(e as T.CVTI2F _)     = LEAF(1, e, [])
                | su(T.CVTF2F(_, _, t))   = su t
                | su(T.FMARK(t, a))       = annotate(su t, a)
                | su(T.FABS(fty, t))      = suUnary(fty, I.FABS, t)
                | su(T.FNEG(fty, t))      = suUnary(fty, I.FCHS, t)
                | su(T.FSQRT(fty, t))     = suUnary(fty, I.FSQRT, t)
                | su(T.FADD(fty, t1, t2)) = suComBinary(fty,FADD,FIADD,t1,t2)
                | su(T.FMUL(fty, t1, t2)) = suComBinary(fty,FMUL,FIMUL,t1,t2)
                | su(T.FSUB(fty, t1, t2)) = suBinary(fty,FSUB,FISUB,t1,t2)
                | su(T.FDIV(fty, t1, t2)) = suBinary(fty,FDIV,FIDIV,t1,t2)
                | su _ = error "su"
         
              (* Try to fold the the memory operand or integer conversion *) 
              and suFold(e as T.FREG _) = (LEAF(0, e, []), false)
                | suFold(e as T.FLOAD _) = (LEAF(0, e, []), false)
                | suFold(e as T.CVTI2F(_,(16 | 32),_)) = (LEAF(0, e, []), true)
                | suFold(T.CVTF2F(_, _, t)) = suFold t
                | suFold(T.FMARK(t, a)) = 
                  let val (t, integer) = suFold t 
                  in  (annotate(t, a), integer) end
                | suFold e = (su e, false)

              (* Form unary tree *)
              and suUnary(fty, funary, t) = 
                  let val t = su t
                  in  UNARY(label t, fty, funary, t, [])
                  end

              (* Form binary tree *)
              and suBinary(fty, binop, ibinop, t1, t2) =
                  let val t1 = su t1
                      val (t2, integer) = suFold t2
                      val n1 = label t1
                      val n2 = label t2
                      val n  = if n1=n2 then n1+1 else Int.max(n1,n2)
                      val myOp = if integer then ibinop else binop
                  in  BINARY(n, fty, myOp, t1, t2, []) 
                  end

              (* Try to fold in the operand if possible. 
               * This only applies to commutative operations.
               *)
              and suComBinary(fty, binop, ibinop, t1, t2) =
                  let val (t1, t2) = if foldableFexp t2 
                                     then (t1, t2) else (t2, t1)
                  in  suBinary(fty, binop, ibinop, t1, t2) end

              and sameTree(LEAF(_, T.FREG(t1,f1), []), 
                           LEAF(_, T.FREG(t2,f2), [])) = 
                        t1 = t2 andalso CB.sameColor(f1,f2)
                | sameTree _ = false

              (* Traverse tree and generate code *)
              fun gencode(LEAF(_, t, an)) = mark(fxld(leafEA t), an)
                | gencode(BINARY(_, _, binop, x, t2 as LEAF(0, y, a1), a2)) = 
                  let val _          = gencode x
                      val (_, fty, src) = leafEA y
                      fun gen(code) = mark(code, a1 @ a2)
                      fun binary(oper32, oper64) =
                          if sameTree(x, t2) then 
                             gen(I.FBINARY{binOp=oper64, src=ST, dst=ST})
                          else
                             let val oper = 
                                   if isMemOpnd src then
                                      case fty of
                                        32 => oper32
                                      | 64 => oper64
                                      | _  => error "gencode: BINARY"
                                   else oper64
                             in gen(I.FBINARY{binOp=oper, src=src, dst=ST}) end
                      fun ibinary(oper16, oper32) =
                          let val oper = case fty of
                                           16 => oper16 
                                         | 32 => oper32 
                                         | _  => error "gencode: IBINARY"
                          in  gen(I.FIBINARY{binOp=oper, src=src}) end
                  in  case binop of
                        FADD => binary(I.FADDS, I.FADDL) 
                      | FSUB => binary(I.FDIVS, I.FSUBL) 
                      | FMUL => binary(I.FMULS, I.FMULL) 
                      | FDIV => binary(I.FDIVS, I.FDIVL) 
                      | FIADD => ibinary(I.FIADDS, I.FIADDL) 
                      | FISUB => ibinary(I.FIDIVS, I.FISUBL) 
                      | FIMUL => ibinary(I.FIMULS, I.FIMULL) 
                      | FIDIV => ibinary(I.FIDIVS, I.FIDIVL) 
                  end  
                | gencode(BINARY(_, fty, binop, t1, t2, an)) = 
                  let fun doit(t1, t2, oper, operP, operRP) = 
                      let (* oper[P] =>  ST(1) := ST oper ST(1); [pop] 
                           * operR[P] => ST(1) := ST(1) oper ST; [pop]
                           *)
                           val n1 = label t1
                           val n2 = label t2
                      in if n1 < n2 andalso n1 <= 7 then 
                           (gencode t2;
                            gencode t1;
                            mark(I.FBINARY{binOp=operP, src=ST, dst=ST1}, an))
                         else if n2 <= n1 andalso n2 <= 7 then
                           (gencode t1;
                            gencode t2;
                            mark(I.FBINARY{binOp=operRP, src=ST, dst=ST1}, an))
                         else 
                         let (* both labels > 7 *)
                             val fs = I.FDirect(newFreg())
                         in  gencode t2;
                             emit(fstp(fty, fs));
                             gencode t1;
                             mark(I.FBINARY{binOp=oper, src=fs, dst=ST}, an)
                         end
                     end
                  in case binop of 
                       FADD => doit(t1,t2,I.FADDL,I.FADDP,I.FADDP)
                     | FMUL => doit(t1,t2,I.FMULL,I.FMULP,I.FMULP)
                     | FSUB => doit(t1,t2,I.FSUBL,I.FSUBP,I.FSUBRP)
                     | FDIV => doit(t1,t2,I.FDIVL,I.FDIVP,I.FDIVRP)
                     | _ => error "gencode.BINARY"
                  end
                | gencode(UNARY(_, _, unaryOp, su, an)) = 
                   (gencode(su); mark(I.FUNARY(unaryOp),an))

              (* Generate code for a leaf.
               * Returns the type and an effective address
               *) 
              and leafEA(T.FREG(fty, f)) = (REAL, fty, I.FDirect f)
                | leafEA(T.FLOAD(fty, ea, mem)) = (REAL, fty, address(ea, mem))
                | leafEA(T.CVTI2F(_, 32, t)) = int2real(32, t)
                | leafEA(T.CVTI2F(_, 16, t)) = int2real(16, t)
                | leafEA(T.CVTI2F(_, 8, t))  = int2real(8, t)
                | leafEA _ = error "leafEA"

              and int2real(ty, e) = 
                  let val (_, ty, ea, cleanup) = convertIntToFloat(ty, e)
                  in  cleanupCode := !cleanupCode @ cleanup;
                      (INTEGER, ty, ea)
                  end

         in  gencode(su fexp);
             emits(!cleanupCode)
          end (*reduceFexp*)


       (*========================================================
        * Tie the two styles of fp code generation together
        *========================================================*)
      and fstore(fty, ea, d, mem, an) = 
          if enableFastFPMode andalso !fast_floating_point 
          then fstore''(fty, ea, d, mem, an)
          else fstore'(fty, ea, d, mem, an)
      and fload(fty, ea, d, mem, an) = 
          if enableFastFPMode andalso !fast_floating_point 
          then fload''(fty, ea, d, mem, an)
          else fload'(fty, ea, d, mem, an)
      and fexpr e = 
          if enableFastFPMode andalso !fast_floating_point 
          then fexpr'' e else fexpr' e
      and doFexpr(fty, e, fd, an) = 
          if enableFastFPMode andalso !fast_floating_point 
          then doFexpr''(fty, e, fd, an)
          else doFexpr'(fty, e, fd, an)

      (* 
       * Compute an integer expression and put the result in 
       * the destination register rd.  
       *)
      and doExpr' disableLEA (exp, rd, an) = 
          let val ty = TRS.size exp
	      val rdOpnd = IntReg ty rd
	      val doExpr = doExpr' false
			   
              fun equalRd(I.Direct (_,r)) = CB.sameColor(r, rd)
                | equalRd _ = false

              (* Makes sure the destination must be a register *)
	      fun dstMustBeReg f = f(rd, rdOpnd)
				   
              (* Generate cmovcc instruction.
               * on Pentium Pro and Pentium II only
                   *)
              fun cmovcc(ty, cc, t1, t2, yes, no) = 
		  let fun genCmov(dstR, _) = 
			  let val _ = doExpr(no, dstR, []) (* false branch *)
			      val cc = cmp(true, ty, cc, t1, t2, [])  (* compare *)
			  in  mark(I.CMOV{cond=cond cc, src=regOrMem(ty, operand ty yes), 
					  dst=dstR}, an) 
			  end
		  in dstMustBeReg genCmov end (* cmovcc *)
        
              fun unknownExp exp = doExpr(Gen.compileRexp exp, rd, an) 

              (* Generate a unary operator *)
              fun unary(ty, unOp, e) = 
		  let val opnd = operand ty e
		  in if isMemOpnd opnd then
			  let val tmp = I.Direct(ty, newReg())
			  in move(ty, opnd, tmp); move(ty, tmp, rdOpnd)
			  end 
                      else move(ty, opnd, rdOpnd);
                      mark(I.UNARY{unOp=unOp ty, opnd=rdOpnd}, an)
		  end
		  
              (* Emit a binary operator.  If the destination is
               * a memReg, do something smarter.
               *)
              fun genBinary(ty, binOp, opnd1, opnd2) =
                  if (isMemOpnd opnd1 orelse isMemOpnd opnd2) orelse
                     equalRd(opnd2)
                  then
                      let val tmpR = newReg()
			  val tmp  = I.Direct (ty,tmpR)
                      in  move(ty, opnd1, tmp);
			  mark(I.BINARY{binOp=binOp ty, src=opnd2, dst=tmp}, an);
			  move(ty, tmp, rdOpnd)
                      end 
                  else
                      (move(ty, opnd1, rdOpnd);
                       mark(I.BINARY{binOp=binOp ty, src=opnd2, dst=rdOpnd}, an)
                      )

              (* Generate a binary operator; it may commute *)
              fun binaryComm(ty, binOp, e1, e2) = 
		  let val (opnd1, opnd2) = 
			  case (operand ty e1, operand ty e2) of
                              (x as I.Immed _, y)      => (y, x)
			    | (x as I.ImmedLabel _, y) => (y, x)
			    | (x, y as I.Direct _)     => (y, x)
			    | (x, y)                   => (x, y)
		  in genBinary(ty, binOp, opnd1, opnd2) end

              (* Generate a binary operator; non-commutative *)
              fun binary(ty, binOp, e1, e2) = 
		  genBinary(ty, binOp, operand ty e1, operand ty e2)

                  (* Add n to rd *)
              fun addN (addOp, n) =
              let val n = operand ty n
              in  mark(I.BINARY{binOp=addOp, src=n, dst=rdOpnd}, an) end

                  (* Generate addition *)
              fun addition(ty, e1, e2) =
                  case e1 of
                    T.REG(_,rs) => if CB.sameColor(rs,rd) then addN (addOp ty, e2)
                                   else addition1(ty, e1,e2)
                  | _ => addition1(ty, e1,e2)
              and addition1(ty, e1, e2) =
                  case e2 of
                    T.REG(_,rs) => if CB.sameColor(rs,rd) then addN (addOp ty, e1) 
                                   else addition2(ty, e1,e2)
                  | _ => addition2(ty, e1,e2) 
              and addition2(ty,e1,e2) =     
                  (if disableLEA then raise EA else
		   dstMustBeReg(fn (dstR, _) => 
				 (case ty
				   of 32 => mark(I.LEA{r32=dstR, 
						      addr=address' 32 (exp, readonly)}, an)
				    | 64 => mark(I.LEAQ{r64=dstR, 
						       addr=address' 64 (exp, readonly)}, an)
				 (* esac *))
				 )
                handle EA => binaryComm(ty, addOp, e1, e2))

              (* Generate shifts; the shift 
               * amount must be a constant or in %rcx *)
              fun shift(ty, opcode, e1, e2) =
              let val (opnd1, opnd2) = (operand ty e1, operand ty e2)
              in  case opnd2 of 
                    I.Immed _ => genBinary(ty, opcode, opnd1, opnd2)
                  | _ => 
                    if equalRd(opnd2) then 
                    let val tmpR = newReg()
                        val tmp  = I.Direct (ty,tmpR)
                    in  move(ty, opnd1, tmp);
                        move(ty, opnd2, rcx ty);
                        mark(I.BINARY{binOp=opcode ty, src=rcx ty, dst=tmp},an);
                        move(ty, tmp, rdOpnd)
                    end
                    else
                        (move(ty, opnd1, rdOpnd);
                         move(ty, opnd2, rcx ty);
                         mark(I.BINARY{binOp=opcode ty, src=rcx ty, dst=rdOpnd},an)
                        )
              end

	      (* division with rounding towards negative infinity *)
	      fun divinf0 (ty, overflow, e1, e2) = let
		  val o1 = operand ty e1
		  val o2 = operand ty e2
		  val l = Label.anon ()
	      in
		  move (ty, o1, rax ty);
		  emit I.CDQ;
		  mark (I.MULTDIV { multDivOp = idivOp ty, src = regOrMem (ty, o2) },
			an);
		  if overflow then trap() else ();
		  app emit [(cmpOp ty) { lsrc = rdx ty, rsrc = I.Immed 0 },
			    I.JCC { cond = I.EQ, opnd = immedLabel l },
			    I.BINARY { binOp = xorOp ty,
				       src = regOrMem (ty, o2),
				       dst = rdx ty },
			    I.JCC { cond = I.GE, opnd = immedLabel l },
			    I.UNARY { unOp = decOp ty, opnd = rax ty }];
		  defineLabel l;
		  move (ty, rax ty, rdOpnd)
	      end

	      (* Division by a power of two when rounding to neginf is the
	       * same as an arithmetic right shift. *)
	      fun divinf (ty, overflow, e1, e2 as T.LI n') =
		  (case analyze n' of
		       (_, NONE) => divinf0 (ty, overflow, e1, e2)
		     | (_, SOME (false, _, p)) =>
		       shift (ty, sarOp, T.REG (ty, expr e1), p)
		     | (_, SOME (true, _, p)) => let
			   val reg = expr e1
		       in
			  emit(I.UNARY { unOp = negOp ty, opnd = I.Direct (ty,reg) });
			  shift (ty, sarOp, T.REG (ty, reg), p)
		       end)
		| divinf (ty, overflow, e1, e2) = divinf0 (ty, overflow, e1, e2)

	      fun reminf0 (ty, e1, e2) = let
		  val o1 = operand ty e1
		  val o2 = operand ty e2
		  val l = Label.anon ()
	      in
		  move (ty, o1, rax ty);
		  emit I.CDQ;
		  mark (I.MULTDIV { multDivOp = idiv1Op ty, src = regOrMem (ty, o2) },
			an);
		  app emit [(cmpOp ty) { lsrc = rdx ty, rsrc = I.Immed 0 },
			    I.JCC { cond = I.EQ, opnd = immedLabel l }];
		  move (ty, rdx ty, rax ty);
		  app emit [I.BINARY { binOp = xorOp ty,
				       src = regOrMem (ty, o2), dst = rax ty },
			    I.JCC { cond = I.GE, opnd = immedLabel l },
			    I.BINARY { binOp = addOp ty,
				       src = regOrMem (ty, o2), dst = rdx ty }];
		  defineLabel l;
		  move (ty, rdx ty, rdOpnd)
	      end

	      (* n mod (power-of-2) corrrsponds to a bitmask (AND). 
	       * If the power is negative, then we must first negate
	       * the argument and then again negate the result. *)
	      fun reminf (ty, e1, e2 as T.LI n') =
		  (case analyze n' of
		       (_, NONE) => reminf0 (ty, e1, e2)
		     | (_, SOME (false, a, _)) =>
		       binaryComm (ty, andOp, e1,
				           T.LI (T.I.fromInt32 (ty, a - 1)))
		     | (_, SOME (true, a, _)) => let
			   val r1 = expr e1
			   val o1 = I.Direct (ty,r1)
		       in
			   emit (I.UNARY { unOp = negOp ty, opnd = o1 });
			   emit (I.BINARY { binOp = andOp ty,
					    src = I.Immed (a - 1),
					    dst = o1 });
			   unary (ty, negOp, T.REG (ty, r1))
		       end)
		| reminf (ty, e1, e2) = reminf0 (ty, e1, e2)

              (* Division or remainder: divisor must be in %rdx:%rax pair *)
              fun divrem(ty, signed, overflow, e1, e2, resultReg) =
		  let val (opnd1, opnd2) = (operand ty e1, operand ty e2)
                      val _ = move(ty, opnd1, rax ty)
                      val oper = if signed then (emit(I.CDQ); idiv1Op ty)
				 else (zero (ty, rdx ty); div1Op ty)
		  in  mark(I.MULTDIV{multDivOp=oper, src=regOrMem (ty, opnd2)},an);
                      move(ty, resultReg, rdOpnd);
                      if overflow then trap() else ()
		  end

              (* Optimize the special case for division *) 
              fun divide (ty, signed, overflow, e1, e2 as T.LI n') =
		  (case analyze n' of
		       (n, SOME (isneg, a, p)) =>
		       if signed then
			   let val label = Label.anon ()
			       val reg1 = expr e1
			       val opnd1 = I.Direct (ty,reg1)
			   in
			       if isneg then
				   emit (I.UNARY { unOp = negOp ty,
						   opnd = opnd1 })
			       else if setZeroBit e1 then ()
			       else emit (cmpOp ty { lsrc = opnd1,
						   rsrc = I.Immed 0 });
			       emit (I.JCC { cond = I.GE,
					     opnd = immedLabel label });
			       emit (if a = 2 then
					 I.UNARY { unOp = incOp ty,
						   opnd = opnd1 }
				     else
					 I.BINARY { binOp = addOp ty,
						    src = I.Immed (a - 1),
						    dst = opnd1 });
			       defineLabel label;
			       shift (ty, sarOp, T.REG (ty, reg1), p)
			   end
		       else shift (ty, shrOp, e1, p)
		     | (n, NONE) =>
		       divrem(ty, signed, overflow andalso (n = ~1 orelse n = 0),
			      e1, e2, rax ty))
		| divide (ty, signed, overflow, e1, e2) =
		  divrem (ty, signed, overflow, e1, e2, rax ty)

	      (* rem never causes overflow *)
              fun rem (ty, signed, e1, e2 as T.LI n') =
		  (case analyze n' of
		       (n, SOME (isneg, a, _)) =>
		       if signed then
			   (* The following logic should work uniformely
			    * for both isneg and not isneg.  It only uses
			    * the absolute value (a) of the divisor.
			    * Here is the formula:
			    *    let p be a power of two and a = abs(p):
			    *
			    *    x % p = x - ((x < 0 ? x + a - 1 : x) & (-a))
			    *
			    * (That's what GCC seems to do.)
			    *)
			   let val r1 = expr e1
			       val o1 = I.Direct (ty,r1)
			       val rt = newReg ()
			       val tmp = I.Direct (ty,rt)
			       val l = Label.anon ()
			   in
			       move (ty, o1, tmp);
			       if setZeroBit e1 then ()
			       else emit ((cmpOp ty) { lsrc = o1,
						   rsrc = I.Immed 0 });
			       emit (I.JCC { cond = I.GE,
					     opnd = immedLabel l });
			       emit (I.BINARY { binOp = addOp ty,
						src = I.Immed (a - 1),
						dst = tmp });
			       defineLabel l;
			       emit (I.BINARY { binOp = andOp ty,
						src = I.Immed (~a),
						dst = tmp });
			       binary (ty, subOp, T.REG (ty, r1), T.REG (ty, rt))
			   end
		       else
			   if isneg then
			       (* this is really strange... *)
			       divrem (ty, false, false, e1, e2, rdx ty)
			   else
			       binaryComm (ty, andOp, e1,
					   T.LI (T.I.fromInt32 (ty, n - 1)))
		     | (_, NONE) => divrem (ty, signed, false, e1, e2, rdx ty))
		| rem(ty, signed, e1, e2) =
                  divrem(ty, signed, false, e1, e2, rdx ty)


	      (* unsigned integer multiplication *)
              fun uMultiply0 (ty, e1, e2) = 
                  (* note e2 can never be (I.Direct rdx) *)
                  (move(ty, operand ty e1, rax ty);
                   mark(I.MULTDIV{multDivOp=mul1Op ty, 
                                  src=regOrMem(ty, operand ty e2)},an);
                   move(ty, rax ty, rdOpnd)
                  )
		  
	      fun uMultiply (ty, e1, e2 as T.LI n') =
		  (case analyze n' of
		       (_, SOME (false, _, p)) => shift (ty, shlOp, e1, p)
		     | _ => uMultiply0 (ty, e1, e2))
		| uMultiply (ty, e1 as T.LI _, e2) = uMultiply (ty, e2, e1)
		| uMultiply (ty, e1, e2) = uMultiply0 (ty, e1, e2)

              (* signed integer multiplication: 
               * The only forms that are allowed that also sets the 
               * OF and CF flags are:
               *
               *          (dst)  (src1)  (src2)
               *      imul r32, r32/m32, imm8
               *          (dst)  (src) 
               *      imul r32, imm8
               *      imul r32, imm32
               *      imul r32, r32/m32
               * Note: destination must be a register!
               *)
              fun multiply (ty, e1, e2) = 
              dstMustBeReg(fn (rd, rdOpnd) =>
              let fun doit(i1 as I.Immed _, i2 as I.Immed _) =
                      (move(ty, i1, rdOpnd);
                       mark(I.BINARY{binOp=imulOp ty, dst=rdOpnd, src=i2},an))
                    | doit(rm, i2 as I.Immed _) = doit(i2, rm)
                    | doit(imm as I.Immed(i), rm) =
                      (case ty
			of 32 => mark(I.MUL3{dst=rd, src1=rm, src2=i},an)
			 | 64 => mark(I.MULQ3{dst=rd, src1=rm, src2=i},an)
		      (* esac *))
                    | doit(r1 as I.Direct _, r2 as I.Direct _) =
                      (move(ty, r1, rdOpnd);
                       mark(I.BINARY{binOp=imulOp ty, dst=rdOpnd, src=r2},an))
                    | doit(r1 as I.Direct _, rm) =
                      (move(ty, r1, rdOpnd);
                       mark(I.BINARY{binOp=imulOp ty, dst=rdOpnd, src=rm},an))
                    | doit(rm, r as I.Direct _) = doit(r, rm)
                    | doit(rm1, rm2) =
                       if equalRd rm2 then
                       let val tmpR = newReg()
                           val tmp  = I.Direct (ty,tmpR)
                       in move(ty, rm1, tmp);
                          mark(I.BINARY{binOp=imulOp ty, dst=tmp, src=rm2},an);
                          move(ty, tmp, rdOpnd)
                       end
                       else
                         (move(ty, rm1, rdOpnd);
                          mark(I.BINARY{binOp=imulOp ty, dst=rdOpnd, src=rm2},an)
                         )
                  val (opnd1, opnd2) = (operand ty e1, operand ty e2)
              in  doit(opnd1, opnd2)
              end
              )

	      fun multiply_notrap (ty, e1, e2 as T.LI n') =
		  (case analyze n' of
		       (_, SOME (isneg, _, p)) => let
			   val r1 = expr e1
			   val o1 = I.Direct (ty,r1)
		       in
			   if isneg then
			       emit (I.UNARY { unOp = negOp ty, opnd = o1 })
			   else ();
			   shift (ty, shlOp, T.REG (ty, r1), p)
		       end
		     | _ => multiply (ty, e1, e2))
		| multiply_notrap (ty, e1 as T.LI _, e2) = multiply_notrap (ty, e2, e1)
		| multiply_notrap (ty, e1, e2) = multiply (ty, e1, e2)

	      fun genLoad(mvOp, ea, mem) = 
                  dstMustBeReg(fn (ty, dst) =>
   			mark(I.MOVE{mvOp=mvOp, src=address (ea, mem), dst=dst},an))
                 (* Generate a zero extended loads *)
              fun load8z32(ea, mem) = genLoad(I.MOVZBL, ea, mem)
              fun load16z32(ea, mem) = genLoad(I.MOVZWL, ea, mem)
              fun load8z64(ea, mem) = genLoad(I.MOVZBQ, ea, mem)
              fun load16z64(ea, mem) = genLoad(I.MOVZWQ, ea, mem)
	      (* sign extended loads*)
              fun load8s64(ea, mem) = genLoad(I.MOVSBQ, ea, mem)
              fun load16s64(ea, mem) = genLoad(I.MOVSWQ, ea, mem)
              fun load8s32(ea, mem) = genLoad(I.MOVSBL, ea, mem)
              fun load16s32(ea, mem) = genLoad(I.MOVSWL, ea, mem)
              fun load32s(ea, mem) = genLoad(I.MOVSLQ, ea, mem)

              fun load32(ea, mem) = genLoad(I.MOVL, ea, mem)
              fun load64(ea, mem) = genLoad(I.MOVQ, ea, mem)

	  in
	      case exp
	       of T.REG(ty,rs) => move'(ty, IntReg ty rs, rdOpnd, an)
		| T.LI z => let
		      val n = toInt32 z
		  in 
		      if n=0 then 
			  mark(I.BINARY{binOp=xorOp ty, src=rdOpnd, dst=rdOpnd}, an)
		      else
			  move'(ty, I.Immed(n), rdOpnd, an)
		  end
		| (T.CONST _ | T.LABEL _) => 
                  move'(ty, I.ImmedLabel exp, rdOpnd, an)
		| T.LABEXP le => move'(ty, I.ImmedLabel le, rdOpnd, an)

		| T.ADD(ty, e1, e2 as T.LI n) => 
		  let val n = toInt32 n
		  in 
		      case n 
		       of 1  => unary(ty, incOp, e1)
			| ~1 => unary(ty, decOp, e1)
			| _ => addition (ty, e1, e2)
		  end
		| T.ADD(ty, e1 as T.LI n, e2) => 
		  let val n = toInt32 n
		  in
		      case n 
		       of  1 => unary(ty, incOp, e2)
			 | ~1 => unary(ty, decOp, e2)
			 | _ => addition (ty, e1, e2)
		  end
		| T.ADD(ty, e1, e2) => addition (ty, e1, e2)
				      
		| T.SUB(ty, e1, e2 as T.LI n) => 
		  let val n = toInt32 n
		  in
		      case n
		       of 0 => doExpr(e1, rd, an)
			| 1 => unary(ty, decOp, e1)
			| ~1 => unary(ty, incOp, e1)
			| _ => binary(ty, subOp, e1, e2)
		  end
		| T.SUB(ty, e1 as T.LI n, e2) => 
	          if n = 0 then unary(ty, negOp, e2)
		  else binary(ty, subOp, e1, e2)
		| T.SUB(ty, e1, e2) => binary(ty, subOp, e1, e2)

		| T.MULU(ty, x, y) => uMultiply(ty, x, y)
		| T.DIVU(ty, x, y) => divide(ty, false, false, x, y)
		| T.REMU(ty, x, y) => rem(ty, false, x, y)

		| T.MULS(ty, x, y) => multiply_notrap (ty, x, y)
		| T.DIVS(T.DIV_TO_ZERO, ty, x, y) => divide(ty, true, false, x, y)
		| T.DIVS(T.DIV_TO_NEGINF, ty, x, y) => divinf (ty, false, x, y)
		| T.REMS(T.DIV_TO_ZERO, ty, x, y) => rem(ty, true, x, y)
		| T.REMS(T.DIV_TO_NEGINF, ty, x, y) => reminf (ty, x, y)

		| T.ADDT(ty, x, y) => (binaryComm(ty, addOp, x, y); trap())
		| T.SUBT(ty, x, y) => (binary(ty, subOp, x, y); trap())
		| T.MULT(ty, x, y) => (multiply (ty, x, y); trap ())
		| T.DIVT(T.DIV_TO_ZERO, ty, x, y) => divide(ty, true, true, x, y)
		| T.DIVT(T.DIV_TO_NEGINF, ty, x, y) => divinf (ty, true, x, y)

		| T.ANDB(ty, x, y) => binaryComm(ty, andOp, x, y)
		| T.ORB(ty, x, y)  => binaryComm(ty, orOp, x, y)
		| T.XORB(ty, x, y) => binaryComm(ty, xorOp, x, y)
		| T.NOTB(ty, x)    => unary(ty, notOp, x)

		| T.SRA(ty, x, y)  => shift(ty, sarOp, x, y)
		| T.SRL(ty, x, y)  => shift(ty, shrOp, x, y)
		| T.SLL(ty, x, y)  => shift(ty, shlOp, x, y)

		| T.LOAD(8, ea, mem) => load8z64(ea, mem)
		| T.LOAD(16, ea, mem) => load16z64(ea, mem)
		| T.LOAD(32, ea, mem) => load32(ea, mem)
		| T.LOAD(64, ea, mem) => load64(ea, mem)

		| T.SX(32,8,T.LOAD(8,ea,mem)) => load8s32(ea, mem)
		| T.SX(32,16,T.LOAD(16,ea,mem)) => load16s32(ea, mem)
		| T.SX(64,8,T.LOAD(8,ea,mem)) => load8s64(ea, mem)
		| T.SX(64,16,T.LOAD(16,ea,mem)) => load16s64(ea, mem)
		| T.SX(64,32,T.LOAD(32,ea,mem)) => load32s(ea, mem)

		| T.ZX(32,8,T.LOAD(8,ea,mem)) => load8z32(ea, mem)
		| T.ZX(32,16,T.LOAD(16,ea,mem)) => load16z32(ea, mem)
		| T.ZX(64,8,T.LOAD(8,ea,mem)) => load8z64(ea, mem)
		| T.ZX(64,16,T.LOAD(16,ea,mem)) => load16z64(ea, mem)
		| T.ZX(64,32,T.LOAD(32,ea,mem)) => load32(ea, mem)

		| T.COND(_, T.CMP(ty, cc, t1, t2), y as T.LI yes, n as T.LI no) =>
		  cmovcc(ty, cc, t1, t2, y, n)
		| T.COND(_, T.CMP(ty, cc, t1, t2), yes, no) => 
		  cmovcc(ty, cc, t1, t2, yes, no)
		| T.LET(s,e) => (doStmt s; doExpr(e, rd, an))
		| T.MARK(e, A.MARKREG f) => (f rd; doExpr(e, rd, an))
		| T.MARK(e, a) => doExpr(e, rd, a::an)
		| T.PRED(e,c) => doExpr(e, rd, A.CTRLUSE c::an)
		| T.REXT e => 
                  ExtensionComp.compileRext (reducer()) {e=e, rd=rd, an=an} 
		(* simplify and try again *)
		| exp => unknownExp exp

	  end (* doExpr' *)

      and doExpr (exp, rd, an) = doExpr' false (exp, rd, an)
      and doExprNoLEA (exp, rd, an) = doExpr' true (exp, rd, an)

      (* generate code for calls *)
      and call(ea, flow, def, use, mem, cutsTo, an, pops) = 
	  let fun return(set, []) = set
		| return(set, a::an) =
		  case #peek A.RETURN_ARG a of 
                      SOME r => return(CB.CellSet.add(r, set), an)
		    | NONE => return(set, an)
	  in
	      mark(I.CALL{opnd=operand 32 ea,defs=cellset(def),uses=cellset(use),
			  return=return(C.empty,an),cutsTo=cutsTo,mem=mem,
			  pops=pops},an)
	  end (* call *)

          (* %eflags <- src *)
      and moveToEflags src =
          if CB.sameColor(src, C.eflags) then ()
          else (move(32, I.Direct (32,src), rax 32); emit(I.LAHF))

      (* dst <- %eflags *) 
      and moveFromEflags dst =
          if CB.sameColor(dst, C.eflags) then ()
          else (emit(I.SAHF); move(32, rax 32, I.Direct (32,dst)))

      (* generate a condition code expression 
       * The zero is for setting the condition code!  
       * I have no idea why this is used.
       *)
      and doCCexpr(T.CMP(ty, cc, t1, t2), rd, an) = 
          (cmp(false, ty, cc, t1, t2, an); 
           moveFromEflags rd
          ) 
        | doCCexpr(T.CC(cond,rs), rd, an) = 
          if CB.sameColor(rs,C.eflags) orelse CB.sameColor(rd,C.eflags) then
              (moveToEflags rs; moveFromEflags rd)
          else
              move'(64, I.Direct (64,rs), I.Direct (64,rd), an)
        | doCCexpr(T.CCMARK(e,A.MARKREG f),rd,an) = (f rd; doCCexpr(e,rd,an))
        | doCCexpr(T.CCMARK(e,a), rd, an) = doCCexpr(e,rd,a::an)
        | doCCexpr(T.CCEXT e, cd, an) = 
          ExtensionComp.compileCCext (reducer()) {e=e, ccd=cd, an=an} 
        | doCCexpr _ = error "doCCexpr"
		       
      and ccExpr e = error "ccExpr"

      (* Compare an expression with zero.
       * On the amd64, TEST is superior to AND for doing the same thing,
       * since it doesn't need to write out the result in a register.
       *)
      and cmpWithZero(cc as (T.EQ | T.NE), e as T.ANDB(ty, a, b), an) = 
          (case ty of
               8  => test(ty, I.TESTB, a, b, an)
             | 16 => test(ty, I.TESTW, a, b, an)
             | 32 => test(ty, I.TESTL, a, b, an)
             | 64 => test(ty, I.TESTQ, a, b, an)
             | _  => doExpr(e, newReg(), an); 
           cc)  
        | cmpWithZero(cc, e, an) = (doExprNoLEA (e, newReg(), an); cc)

      (* Emit a test.
       *   The available modes are
       *      r/m, r
       *      r/m, imm
       * On selecting the right instruction: TESTL/TESTW/TESTB.   
       * When anding an operand with a constant
       * that fits within 8 (or 16) bits, it is possible to use TESTB,
       * (or TESTW) instead of TESTL.   Because amd64 is little endian, 
       * this works for memory operands too.  However, with TESTB, it is
       * not possible to use registers other than 
       * AL, CL, BL, DL, and AH, CH, BH, DH.  So, the best way is to
       * perform register allocation first, and if the operand registers
       * are one of RAX, RCX, EBX, or RDX, replace the TESTL instruction 
       * by TESTB.
       *)
      and test(ty, testopcode, a, b, an) = 
          let val (_, opnd1, opnd2) = commuteComparison(ty, T.EQ, true, a, b)
              (* translate r, r/m => r/m, r *)
              val (opnd1, opnd2) = 
                  if isMemOpnd opnd2 then (opnd2, opnd1) else (opnd1, opnd2)
          in  mark(testopcode{lsrc=opnd1, rsrc=opnd2}, an)
          end
	  
      (* generate a real comparison; return the real cc used *)
      and genCmp(ty, swapable, cc, a, b, an) = 
          let val (cc, opnd1, opnd2) = commuteComparison(ty, cc, swapable, a, b)
          in 
	      (case ty 
		of 8 => mark(I.CMPB{lsrc=opnd1, rsrc=opnd2}, an)
		 | 16 => mark(I.CMPW{lsrc=opnd1, rsrc=opnd2}, an)
		 | 32 => mark(I.CMPL{lsrc=opnd1, rsrc=opnd2}, an)
		 | 64 => mark(I.CMPQ{lsrc=opnd1, rsrc=opnd2}, an)
	      (* esac *));
	      cc 
          end


      (* Give a and b which are the operands to a comparison (or test)
       * Return the appropriate condition code and operands.
       *   The available modes are:
       *        r/m, imm
       *        r/m, r
       *        r,   r/m
       *)
      and commuteComparison(ty, cc, swapable, a, b) = 
          let val (opnd1, opnd2) = (operand ty a, operand ty b)
          in  (* Try to fold in the operands whenever possible *)
              case (isImmediate opnd1, isImmediate opnd2) of
                  (true, true) => (cc, moveToReg (ty, opnd1), opnd2)
		| (true, false) => 
                  if swapable then (T.Basis.swapCond cc, opnd2, opnd1)
                  else (cc, moveToReg (ty, opnd1), opnd2)
		| (false, true) => (cc, opnd1, opnd2)
		| (false, false) => 
                  (case (opnd1, opnd2) of
                       (_, I.Direct _) => (cc, opnd1, opnd2)
                     | (I.Direct _, _) => (cc, opnd1, opnd2)
                     | (_, _)          => (cc, moveToReg (ty, opnd1), opnd2)
                  )
          end  (* commuteComparison *)
	  
      (* generate a comparison and sets the condition code;
       * return the actual cc used.  If the flag swapable is true,
       * we can also reorder the operands. 
       *)
      and cmp(swapable, ty, cc, t1, t2, an) = 
          (* == and <> can be always be reordered *)
          let val swapable = swapable orelse cc = T.EQ orelse cc = T.NE
          in (* Sometimes the comparison is not necessary because
              * the bits are already set! 
              *)
             if isZero t1 andalso setZeroBit2 t2 then 
                 if swapable then
                    cmpWithZero(T.Basis.swapCond cc, t2, an)
                 else (* can't reorder the comparison! *)
                    genCmp(ty, false, cc, t1, t2, an)
             else if isZero t2 andalso setZeroBit2 t1 then 
                cmpWithZero(cc, t1, an)
             else genCmp(ty, swapable, cc, t1, t2, an) 
          end

      (* generate code for branching *)
      and branch(T.CMP(ty, cc, t1, t2), lab, an) =
          (* allow reordering of operands *)
          let val cc = cmp(true, ty, cc, t1, t2, []) 
          in  mark(I.JCC{cond=cond cc, opnd=immedLabel lab}, an) end
        | branch(T.FCMP(fty, fcc, t1, t2), lab, an) = 
          fbranch(fty, fcc, t1, t2, lab, an)
        | branch(ccexp, lab, an) =
          (doCCexpr(ccexp, C.eflags, []);
           mark(I.JCC{cond=cond(Gen.condOf ccexp), opnd=immedLabel lab}, an)
          )

      and store8(ea, d, mem, an) = 
	  mark(I.MOVE{mvOp=I.MOVB, src=immedOrReg(16, operand 16 d), dst=address (ea, mem)}, an)
      and store16(ea, d, mem, an) = 
	  mark(I.MOVE{mvOp=I.MOVW, src=immedOrReg(16, operand 16 d), dst=address (ea, mem)}, an)
      and store32(ea, d, mem, an) = 
          move'(32, immedOrReg(32, operand 32 d), address (ea, mem), an)
      and store64(ea, d, mem, an) = 
          move'(64, immedOrReg(64, operand 64 d), address (ea, mem), an)


      (*================================================================
       * Optimizations for x := x op y 
       * Special optimizations: 
       * Generate a binary operator, result must in memory.
       * The source must not be in memory
       *================================================================*)
      and binaryMem(ty, binOp, src, dst, mem, an) =
          mark(I.BINARY{binOp=binOp, src=immedOrReg(ty, operand ty src),
                        dst=address (dst,mem)}, an)
      and unaryMem(ty, unOp, opnd, mem, an) =
          mark(I.UNARY{unOp=unOp, opnd=address (opnd,mem)}, an)

      and isOne(T.LI n) = n = 1
        | isOne _ = false

      (* 
       * Perform optimizations based on recognizing 
       *    x := x op y    or
       *    x := y op x 
       * first.
       *)
      and store(ty, ea, d, mem, an, 
                {INC,DEC,ADD,SUB,NOT,NEG,SHL,SHR,SAR,OR,AND,XOR, ...},
                doStore
               ) = 
          let fun default() = doStore(ea, d, mem, an)
              fun binary1(t, t', unary, binary, ea', x) =  
                  if t = ty andalso t' = ty then
                     if MLTreeUtils.eqRexp(ea, ea') then
                        if isOne x then unaryMem(ty, unary, ea, mem, an)
                        else binaryMem(ty, binary, x, ea, mem, an)
                      else default()
                  else default()
              fun unary(t,unOp, ea') = 
                  if t = ty andalso MLTreeUtils.eqRexp(ea, ea') then
                     unaryMem(ty, unOp, ea, mem, an)
                  else default() 
              fun binary(t,t',binOp,ea',x) =
                  if t = ty andalso t' = ty andalso
                     MLTreeUtils.eqRexp(ea, ea') then
                      binaryMem(ty, binOp, x, ea, mem, an)
                  else default()

              fun binaryCom1(t,unOp,binOp,x,y) = 
              if t = ty then
              let fun again() =
                    case y of
                      T.LOAD(ty',ea',_) =>
                        if ty' = ty andalso MLTreeUtils.eqRexp(ea, ea') then
                           if isOne x then unaryMem(ty, unOp, ea, mem, an)
                           else binaryMem(ty, binOp,x,ea,mem,an)
                        else default()
                    | _ => default()
              in case x of 
                    T.LOAD(ty',ea',_) =>
                      if ty' = ty andalso MLTreeUtils.eqRexp(ea, ea') then
                         if isOne y then unaryMem(ty, unOp, ea, mem, an)
                         else binaryMem(ty, binOp,y,ea,mem,an)
                      else again()
                  | _ => again()
              end 
              else default()

              fun binaryCom(t,binOp,x,y) = 
              if t = ty then
              let fun again() =
                    case y of
                      T.LOAD(ty',ea',_) =>
                        if ty' = ty andalso MLTreeUtils.eqRexp(ea, ea') then
                           binaryMem(ty, binOp,x,ea,mem,an)
                        else default()
                    | _ => default()
              in  case x of 
                    T.LOAD(ty',ea',_) =>
                      if ty' = ty andalso MLTreeUtils.eqRexp(ea, ea') then
                         binaryMem(ty, binOp,y,ea,mem,an)
                      else again()
                  | _ => again()
              end 
              else default()

          in  
	      case d of
                  T.ADD(t,x,y) => binaryCom1(t,INC,ADD,x,y)
		| T.SUB(t,T.LOAD(t',ea',_),x) => binary1(t,t',DEC,SUB,ea',x)
		| T.ORB(t,x,y) => binaryCom(t,OR,x,y)
		| T.ANDB(t,x,y) => binaryCom(t,AND,x,y)
		| T.XORB(t,x,y) => binaryCom(t,XOR,x,y)
		| T.SLL(t,T.LOAD(t',ea',_),x) => binary(t,t',SHL,ea',x)
		| T.SRL(t,T.LOAD(t',ea',_),x) => binary(t,t',SHR,ea',x)
		| T.SRA(t,T.LOAD(t',ea',_),x) => binary(t,t',SAR,ea',x)
		| T.NEG(t,T.LOAD(t',ea',_)) => unary(t,NEG,ea')
		| T.NOTB(t,T.LOAD(t',ea',_)) => unary(t,NOT,ea')
		| _ => default()
          end (* store *)

      (* generate code for jumps *)
      and jmp(lexp as T.LABEL lab, labs, an) = 
          mark(I.JMP(I.ImmedLabel lexp, [lab]), an)
        | jmp(T.LABEXP le, labs, an) = mark(I.JMP(I.ImmedLabel le, labs), an)
        | jmp(ea, labs, an)          = mark(I.JMP(operand defaultAddrTy ea, labs), an)
				       
      and stmt(T.MV(_, rd, e), an) = doExpr (e, rd, an)
        | stmt(T.FMV(fty, fd, e), an) = doFexpr(fty, e, fd, an) 
        | stmt(T.CCMV(ccd, e), an) = doCCexpr(e, ccd, an) 
        | stmt(T.COPY(ty, dst, src), an) = copy(ty, dst, src, an)
        | stmt(T.FCOPY(fty, dst, src), an) = fcopy(fty, dst, src, an)
        | stmt(T.JMP(e, labs), an) = jmp(e, labs, an)
        | stmt(T.CALL{funct, targets, defs, uses, region, pops, ...}, an) = 
             call(funct,targets,defs,uses,region,[],an, pops)
        | stmt(T.FLOW_TO(T.CALL{funct, targets, defs, uses, region, pops, ...},
                         cutTo), an) = 
             call(funct,targets,defs,uses,region,cutTo,an, pops)
        | stmt(T.RET _, an) = mark(I.RET NONE, an)
        | stmt(T.STORE(8, ea, d, mem), an)  = 
             store(8, ea, d, mem, an, opcodes8, store8)
        | stmt(T.STORE(16, ea, d, mem), an) = 
             store(16, ea, d, mem, an, opcodes16, store16)
        | stmt(T.STORE(32, ea, d, mem), an) = 
             store(32, ea, d, mem, an, opcodes32, store32)
        | stmt(T.STORE(64, ea, d, mem), an) = 
             store(64, ea, d, mem, an, opcodes64, store64)

        | stmt(T.FSTORE(fty, ea, d, mem), an) = fstore(fty, ea, d, mem, an)
        | stmt(T.BCC(cc, lab), an) = branch(cc, lab, an)
        | stmt(T.DEFINE l, _) = defineLabel l
        | stmt(T.LIVE S, an) = mark'(I.LIVE{regs=cellset S,spilled=C.empty},an)
        | stmt(T.KILL S, an) = mark'(I.KILL{regs=cellset S,spilled=C.empty},an)
        | stmt(T.ANNOTATION(s, a), an) = stmt(s, a::an)
        | stmt(T.EXT s, an) =
             ExtensionComp.compileSext (reducer()) {stm=s, an=an} 
        | stmt(s, _) = doStmts(Gen.compileStm s)

      and doStmt s = stmt(s, [])
      and doStmts ss = app doStmt ss

      and beginCluster' _ =
         ((* Must be cleared by the client.
           * if rewriteMemReg then memRegsUsed := 0w0 else (); 
           *)
          floatingPointUsed := false;
          trapLabel := NONE; 
          beginCluster 0
         )
      and endCluster' a =
         (case !trapLabel
          of NONE => ()
           | SOME(_, lab) => (defineLabel lab; emit(I.INTO))
          (*esac*);
          (* If floating point has been used allocate an extra
           * register just in case we didn't use any explicit register
           *)
          if !floatingPointUsed then (newFreg(); ())
          else ();
          endCluster a
         )

      (* generate floating point expression and put the result in fd *)
      and doFexpr'(fty, T.FREG(_, fs), fd, an) = 
            (if CB.sameColor(fs,fd) then () 
             else mark'(I.COPY{k=CB.FP, sz=64, dst=[fd], src=[fs], tmp=NONE}, an)
            )
        | doFexpr'(_, T.FLOAD(fty, ea, mem), fd, an) = 
            fload'(fty, ea, mem, fd, an)
        | doFexpr'(fty, T.FEXT fexp, fd, an) = 
            (ExtensionComp.compileFext (reducer()) {e=fexp, fd=fd, an=an};
             if CB.sameColor(fd,ST0) then () else emit(fstp(fty, I.FDirect fd))
            )
        | doFexpr'(fty, e, fd, an) =
            (reduceFexp(fty, e, []);
             if CB.sameColor(fd,ST0) then ()
             else mark(fstp(fty, I.FDirect fd), an)
            )

      and fexpr''(e as T.FREG(_,f)) = 
          if isFMemReg f then transFexpr e else f
        | fexpr'' e = transFexpr e

      (* generate code for floating point compare and branch *)
      and fbranch(fty, fcc, t1, t2, lab, an) = 
          let fun j cc = mark(I.JCC{cond=cc, opnd=immedLabel lab},an)
          in  fbranching(fty, fcc, t1, t2, j)
          end

      and fbranching(fty, fcc, t1, t2, j) = 
          let fun ignoreOrder (T.FREG _) = true
                | ignoreOrder (T.FLOAD _) = true
                | ignoreOrder (T.FMARK(e,_)) = ignoreOrder e
                | ignoreOrder _ = false

              fun compare'() = (* Sethi-Ullman style *)
                  (if ignoreOrder t1 orelse ignoreOrder t2 then 
                        (reduceFexp(fty, t2, []); reduceFexp(fty, t1, []))
                   else (reduceFexp(fty, t1, []); reduceFexp(fty, t2, []); 
                         emit(I.FXCH{opnd=C.ST(1)}));
                   emit(I.FUCOMPP);
                   fcc
                  )

              fun compare''() = 
                      (* direct style *)
                      (* Try to make lsrc the memory operand *)
                  let val lsrc = foperand(fty, t1)
                      val rsrc = foperand(fty, t2)
                      val fsize = fsize fty
                      fun cmp(lsrc, rsrc, fcc) =
			  (emit(I.FCMP{i=true,fsize=fsize,lsrc=lsrc,rsrc=rsrc});
                           fcc)
                  in  case (lsrc, rsrc) of
                         (I.FPR _, I.FPR _) => cmp(lsrc, rsrc, fcc)
                       | (I.FPR _, mem) => cmp(mem,lsrc,T.Basis.swapFcond fcc)
                       | (mem, I.FPR _) => cmp(lsrc, rsrc, fcc)
                       | (lsrc, rsrc) => (* can't be both memory! *)
                         let val ftmpR = newFreg()
                             val ftmp  = I.FPR ftmpR
                         in  emit(I.FMOVE{fsize=fsize,src=rsrc,dst=ftmp});
                             cmp(lsrc, ftmp, fcc)
                         end
                  end

              fun compare() = 
                  if enableFastFPMode andalso !fast_floating_point 
                  then compare''() else compare'()

              fun andil i = emit(I.BINARY{binOp=I.ANDL,src=I.Immed(i),dst=rax 32})
              fun testil i = emit(I.TESTL{lsrc=rax 32,rsrc=I.Immed(i)})
              fun xoril i = emit(I.BINARY{binOp=I.XORL,src=I.Immed(i),dst=rax 32})
              fun cmpil i = emit(I.CMPL{rsrc=I.Immed(i), lsrc=rax 32})
              fun sahf() = emit(I.SAHF)
              fun branch(fcc) =
                  case fcc
                  of T.==   => (andil 0x4400; xoril 0x4000; j(I.EQ))
                   | T.?<>  => (andil 0x4400; xoril 0x4000; j(I.NE))
                   | T.?    => (sahf(); j(I.P))
                   | T.<=>  => (sahf(); j(I.NP))
                   | T.>    => (testil 0x4500;  j(I.EQ))
                   | T.?<=  => (testil 0x4500;  j(I.NE))
                   | T.>=   => (testil 0x500; j(I.EQ))
                   | T.?<   => (testil 0x500; j(I.NE))
                   | T.<    => (andil 0x4500; cmpil 0x100; j(I.EQ))
                   | T.?>=  => (andil 0x4500; cmpil 0x100; j(I.NE))
                   | T.<=   => (andil 0x4100; cmpil 0x100; j(I.EQ);
                                cmpil 0x4000; j(I.EQ))
                   | T.?>   => (sahf(); j(I.P); testil 0x4100; j(I.EQ))
                   | T.<>   => (testil 0x4400; j(I.EQ))
                   | T.?=   => (testil 0x4400; j(I.NE))
                   | _      => error(concat[
				  "fbranch(", T.Basis.fcondToString fcc, ")"
				])
                 (*esac*)

              (*
               *             P  Z  C
               * x < y       0  0  1
               * x > y       0  0  0
               * x = y       0  1  0
               * unordered   1  1  1
               * When it's unordered, all three flags, P, Z, C are set.
               *)
                
              fun fast_branch(fcc) =
                  case fcc
                  of T.==   => orderedOnly(I.EQ)
                   | T.?<>  => (j(I.P); j(I.NE))
                   | T.?    => j(I.P)
                   | T.<=>  => j(I.NP)
                   | T.>    => orderedOnly(I.A)
                   | T.?<=  => j(I.BE)
                   | T.>=   => orderedOnly(I.AE)
                   | T.?<   => j(I.B)
                   | T.<    => orderedOnly(I.B)
                   | T.?>=  => (j(I.P); j(I.AE))
                   | T.<=   => orderedOnly(I.BE)
                   | T.?>   => (j(I.P); j(I.A))
                   | T.<>   => orderedOnly(I.NE)
                   | T.?=   => j(I.EQ)
                   | _      => error(concat[
				  "fbranch(", T.Basis.fcondToString fcc, ")"
				])
                 (*esac*)
              and orderedOnly fcc =
              let val label = Label.anon()
              in  emit(I.JCC{cond=I.P, opnd=immedLabel label});
                  j fcc;
                  defineLabel label
              end
 
              val fcc = compare() 
          in  if (*!arch <> Pentium andalso*)
                 (enableFastFPMode andalso !fast_floating_point) then
                fast_branch(fcc)
              else
                (emit I.FNSTSW;   
                 branch(fcc)
                )
          end

       (*========================================================
        * This section generates 3-address style floating 
        * point code.  
        *========================================================*)

      and isize 16 = I.I16
        | isize 32 = I.I32
        | isize _  = error "isize"

      and fstore''(fty, ea, d, mem, an) = 
          (floatingPointUsed := true;
           mark(I.FMOVE{fsize=fsize fty, dst=address(ea,mem), 
                        src=foperand(fty, d)},
                an)
          )

      and fload''(fty, ea, mem, d, an) = 
          (floatingPointUsed := true;
           mark(I.FMOVE{fsize=fsize fty, src=address(ea,mem), 
                        dst=RealReg d}, an)
          )

      and fiload''(ity, ea, d, an) = 
          (floatingPointUsed := true;
           mark(I.FILOAD{isize=isize ity, ea=ea, dst=RealReg d}, an)
          )

         (* 
          * Process a floating point operand.  Put operand in register 
          * when possible.  The operand should match the given fty.
          *)
      and foperand(fty, e as T.FREG(fty', f)) = 
             if fty = fty' then RealReg f else I.FPR(fexpr'' e)
        | foperand(fty, T.CVTF2F(_, _, e)) =
             foperand(fty, e) (* nop on the amd64 *)
        | foperand(fty, e as T.FLOAD(fty', ea, mem)) = 
             (* fold operand when the precison matches *)
             if fty = fty' then address(ea, mem) else I.FPR(fexpr'' e)
        | foperand(fty, e) = I.FPR(fexpr'' e)

         (* 
          * Process a floating point operand. 
          * Try to fold in a memory operand or conversion from an integer.
          *)
      and fioperand(T.FREG(fty,f)) = (REAL, fty, RealReg f, [])
        | fioperand(T.FLOAD(fty, ea, mem)) = 
             (REAL, fty, address(ea, mem), [])
        | fioperand(T.CVTF2F(_, _, e)) = fioperand(e) (* nop on the amd64 *)
        | fioperand(T.CVTI2F(_, ty, e)) = convertIntToFloat(ty, e)
        | fioperand(T.FMARK(e,an)) = fioperand(e) (* XXX *)
        | fioperand(e) = (REAL, 64, I.FPR(fexpr'' e), [])

          (* Generate binary operator.  Since the real binary operators
           * does not take memory as destination, we also ensure this 
           * does not happen.  
           *)
      and fbinop(targetFty, 
                 binOp, binOpR, ibinOp, ibinOpR, lsrc, rsrc, fd, an) = 
              (* Put the mem operand in rsrc *)
          let 
              fun isMemOpnd(T.FREG(_, f)) = isFMemReg f
                | isMemOpnd(T.FLOAD _) = true
                | isMemOpnd(T.CVTI2F(_, (16 | 32), _)) = true
                | isMemOpnd(T.CVTF2F(_, _, t)) = isMemOpnd t
                | isMemOpnd(T.FMARK(t, _)) = isMemOpnd t
                | isMemOpnd _ = false
              val (binOp, ibinOp, lsrc, rsrc) = 
                  if isMemOpnd lsrc then (binOpR, ibinOpR, rsrc, lsrc)
                  else (binOp, ibinOp, lsrc, rsrc)
              val lsrc = foperand(targetFty, lsrc)
              val (kind, fty, rsrc, code) = fioperand(rsrc)
              fun dstMustBeFreg f =
                  if targetFty <> 64 then
                  let val tmpR = newFreg() 
                      val tmp  = I.FPR tmpR
                  in  mark(f tmp, an); 
                      emit(I.FMOVE{fsize=fsize targetFty, 
                                   src=tmp, dst=RealReg fd})
                  end
                  else mark(f(RealReg fd), an)
          in  case kind of
                REAL => 
                  dstMustBeFreg(fn dst => 
                                   I.FBINOP{fsize=fsize fty, binOp=binOp, 
                                            lsrc=lsrc, rsrc=rsrc, dst=dst}) 
              | INTEGER => 
                  (dstMustBeFreg(fn dst =>
                                    I.FIBINOP{isize=isize fty, binOp=ibinOp, 
                                              lsrc=lsrc, rsrc=rsrc, dst=dst});
                   emits code
                  )
          end

      and transFexpr e = 
          let val fd = newFreg() in doFexpr''(64, e, fd, []); fd end

      and doFexpr''(fty, e, fd, an) = 
         (floatingPointUsed := true;
          case e of
            T.FREG(_,fs) => if CB.sameColor(fs,fd) then () 
                            else fcopy''(fty, [fd], [fs], an)
            (* Stupid amd64 does everything as 80-bits internally. *)

            (* Binary operators *)
          | T.FADD(_, a, b) => fbinop(fty, 
                                      I.FADDL, I.FADDL, I.FIADDL, I.FIADDL, 
                                      a, b, fd, an)
          | T.FSUB(_, a, b) => fbinop(fty,
                                      I.FSUBL, I.FSUBRL, I.FISUBL, I.FISUBRL,
                                      a, b, fd, an)
          | T.FMUL(_, a, b) => fbinop(fty,
                                      I.FMULL, I.FMULL, I.FIMULL, I.FIMULL,
                                      a, b, fd, an)
          | T.FDIV(_, a, b) => fbinop(fty,
                                      I.FDIVL, I.FDIVRL, I.FIDIVL, I.FIDIVRL,
                                      a, b, fd, an)

            (* Unary operators *)
          | T.FNEG(_, a) => funop(fty, I.FCHS, a, fd, an)
          | T.FABS(_, a) => funop(fty, I.FABS, a, fd, an)
          | T.FSQRT(_, a) => funop(fty, I.FSQRT, a, fd, an)

            (* Load *)
          | T.FLOAD(fty,ea,mem) => fload''(fty, ea, mem, fd, an)

            (* Type conversions *)
          | T.CVTF2F(_, _, e) => doFexpr''(fty, e, fd, an)
          | T.CVTI2F(_, ty, e) => 
            let val (_, ty, ea, cleanup) = convertIntToFloat(ty, e)
            in  fiload''(ty, ea, fd, an); 
                emits cleanup
            end

          | T.FMARK(e,A.MARKREG f) => (f fd; doFexpr''(fty, e, fd, an))
          | T.FMARK(e, a) => doFexpr''(fty, e, fd, a::an)
          | T.FPRED(e, c) => doFexpr''(fty, e, fd, A.CTRLUSE c::an)
          | T.FEXT fexp =>
             ExtensionComp.compileFext (reducer()) {e=fexp, fd=fd, an=an}
          | _ => error("doFexpr''")
         )

      and funop(fty, unOp, src, fd, an) = 
          let val src = foperand(fty, src)
          in  mark(I.FUNOP{fsize=fsize fty,
                           unOp=unOp, src=src, dst=RealReg fd},an)
          end

      and reducer() = 
          TS.REDUCER{reduceRexp    = expr,
                     reduceFexp    = fexpr,
                     reduceCCexp   = ccExpr,
                     reduceStm     = stmt,
		     (* FIXME: what is needed for the operand ty? *)
                     operand       = operand defaultAddrTy,
                     reduceOperand = (fn x => reduceOpnd (defaultAddrTy, x)),
                     addressOf     = fn e => address(e, I.Region.memory), (*XXX*)
                     emit          = mark',
                     instrStream   = instrStream, 
                     mltreeStream  = self() 
                    }
	  
      and self() =
          TS.S.STREAM
              {  beginCluster   = beginCluster',
		 endCluster     = endCluster',
		 emit           = doStmt,
		 pseudoOp       = pseudoOp,
		 defineLabel    = defineLabel,
		 entryLabel     = entryLabel,
		 comment        = comment,
		 annotation     = annotation,
		 getAnnotations = getAnnotations,
		 exitBlock      = exitBlock o cellset 
              }
      in 
	  self ()
      end (* selectInstructions *)

end (* AMD64 *)
end (* local *)
