(* X86.sml -- pattern matching version of x86 instruction set generation. 
 *
 * COPYRIGHT (c) 1998 Bell Laboratories.
 * 
 * This is a revised version that takes into account of
 * the extended x86 instruction set, and has better handling of
 * non-standard types.  I've factored out the integer/floating point 
 * comparison code, added optimizations for conditional moves. 
 * The latter generates SETcc and CMOVcc (Pentium Pro only) instructions. 
 * To avoid problems, I have tried to incorporate as much of 
 * Lal's original magic incantations as possible.
 *
 * Some changes:
 *
 *  1.  REMU/REMS/REMT are now supported 
 *  2.  COND is supported by generating SETcc and/or CMOVcc; this
 *      may require at least a Pentium II to work.
 *  3.  Division by a constant has been optimized.   Division by
 *      a power of 2 generates SHRL or SARL.
 *  4.  Better addressing mode selection has been implemented.  This should
 *      improve array indexing on SML/NJ.
 *  5.  Generate testl/testb instead of andl whenever appropriate.  This
 *      is recommended by the Intel Optimization Guide and seems to improve
 *      boxity tests on SML/NJ.
 * -- Allen
 *)
local
   val rewriteMemReg = true (* should we rewrite memRegs *)
in

functor X86
  (structure X86Instr : X86INSTR
   structure X86MLTree : MLTREE
   structure ExtensionComp : MLTREE_EXTENSION_COMP
     where I = X86Instr and T = X86MLTree
     sharing X86MLTree.Region = X86Instr.Region
     sharing X86MLTree.LabelExp = X86Instr.LabelExp
    datatype arch = Pentium | PentiumPro | PentiumII | PentiumIII
    val arch : arch ref
    val tempMem : X86Instr.operand (* temporary for CVTI2F *)
  ) : sig include MLTREECOMP 
          val rewriteMemReg : bool
      end = 
struct
  structure T = X86MLTree
  structure S = T.Stream
  structure I = X86Instr
  structure C = I.C
  structure Shuffle = Shuffle(I)
  structure W32 = Word32
  structure LE = I.LabelExp
  structure A = MLRiscAnnotations

  type instrStream = (I.instruction,C.regmap,C.cellset) T.stream
  type mltreeStream = (T.stm,C.regmap,T.mlrisc list) T.stream
 
  structure Gen = MLTreeGen
     (structure T = T
      val intTy = 32
      val naturalWidths = [32]
      datatype rep = SE | ZE | NEITHER
      val rep = NEITHER
     )

  fun error msg = MLRiscErrorMsg.error("X86",msg)

  (* Should we perform automatic MemReg translation?  
   * If this is on, we can avoid doing RewritePseudo phase entirely.
   *)
  val rewriteMemReg = rewriteMemReg
  fun isMemReg r = rewriteMemReg andalso r >= 8 andalso r < 32

  val ST0 = C.ST 0
  val ST7 = C.ST 7

  (* 
   * The code generator 
   *)
  fun selectInstructions 
       (instrStream as
        S.STREAM{emit,defineLabel,entryLabel,pseudoOp,annotation,
                 beginCluster,endCluster,exitBlock,alias,phi,comment,...}) =
  let exception EA

      (* label where a trap is generated -- one per cluster *)
      val trapLabel = ref (NONE: (I.instruction * Label.label) option)

      (* effective address of an integer register *)
      fun IntReg r = if isMemReg r then MemReg r else I.Direct r
      and MemReg r = 
          ((* memRegsUsed := Word.orb(!memRegsUsed, 
                            Word.<<(0w1, Word.fromInt r-0w8)); *)
           I.MemReg r
          ) 

      (* Add an overflow trap *)
      fun trap() =
      let val jmp = 
            case !trapLabel of 
              NONE => let val label = Label.newLabel "trap"
                          val jmp   = I.JCC{cond=I.O, 
                                            opnd=I.ImmedLabel(LE.LABEL label)}
                      in  trapLabel := SOME(jmp, label); jmp end
            | SOME(jmp, _) => jmp
      in  emit jmp end

      val newReg  = C.newReg
      val newFreg = C.newFreg

      (* mark an expression with a list of annotations *) 
      fun mark'(i,[]) = i 
        | mark'(i,a::an) = mark'(I.ANNOTATION{i=i,a=a},an) 

      (* annotate an expression and emit it *)
      fun mark(i,an) = emit(mark'(i,an))

      (* emit parallel copies for integers 
       * Translates parallel copies that involve memregs into 
       * individual copies.
       *)
      fun copy([], [], an) = ()
        | copy(dst, src, an) = 
          let fun mvInstr{dst as I.MemReg rd, src as I.MemReg rs} = 
                  if rd = rs then [] else
                  let val tmpR = I.Direct(newReg())
                  in  [I.MOVE{mvOp=I.MOVL, src=src, dst=tmpR},
                       I.MOVE{mvOp=I.MOVL, src=tmpR, dst=dst}]
                  end
                | mvInstr{dst=I.Direct rd, src=I.Direct rs} = 
                    if rd = rs then [] 
                    else [I.COPY{dst=[rd], src=[rs], tmp=NONE}]
                | mvInstr{dst, src} = [I.MOVE{mvOp=I.MOVL, src=src, dst=dst}]
          in
             app emit (Shuffle.shuffle{mvInstr=mvInstr, ea=IntReg}
               {regmap=fn r => r, tmp=SOME(I.Direct(newReg())),
                dst=dst, src=src})
          end
 
      (* conversions *)
      val itow = Word.fromInt
      val wtoi = Word.toInt
      fun toInt32 i = Int32.fromLarge(Int.toLarge i)
      val w32toi32 = Word32.toLargeIntX 
      val i32tow32 = Word32.fromLargeInt

      (* One day, this is going to bite us when precision(LargeInt)>32 *)
      fun wToInt32 w = Int32.fromLarge(Word32.toLargeIntX w)

      (* some useful registers *)
      val eax = I.Direct(C.eax)
      val ecx = I.Direct(C.ecx)
      val edx = I.Direct(C.edx)

      fun immedLabel lab = I.ImmedLabel(LE.LABEL lab)
 
      (* Is the expression zero? *)
      fun isZero(T.LI 0) = true
        | isZero(T.LI32 0w0) = true
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
        | setZeroBit(T.MARK(e, _)) = setZeroBit e
        | setZeroBit _             = false

      (* emit parallel copies for floating point *)
      fun fcopy(fty, [], [], _) = ()
        | fcopy(fty, dst as [_], src as [_], an) = 
            mark(I.FCOPY{dst=dst,src=src,tmp=NONE}, an)
        | fcopy(fty, dst, src, an) = 
            mark(I.FCOPY{dst=dst,src=src,tmp=SOME(I.FDirect(newFreg()))}, an)

      (* Translates MLTREE condition code to x86 condition code *)
      fun cond T.LT = I.LT | cond T.LTU = I.B
        | cond T.LE = I.LE | cond T.LEU = I.BE
        | cond T.EQ = I.EQ | cond T.NE  = I.NE
        | cond T.GE = I.GE | cond T.GEU = I.AE
        | cond T.GT = I.GT | cond T.GTU = I.A

      (* Move and annotate *) 
      fun move'(src as I.Direct s, dst as I.Direct d, an) =
          if s=d then ()
          else mark(I.COPY{dst=[d], src=[s], tmp=NONE}, an)
        | move'(src, dst, an) = mark(I.MOVE{mvOp=I.MOVL, src=src, dst=dst}, an)

      (* Move only! *)  
      fun move(src, dst) = move'(src, dst, [])

      fun zero dst = emit(I.BINARY{binOp=I.XORL, src=dst, dst=dst})

      val readonly = I.Region.readonly

      (* 
       * Compute an effective address.  This is a new version
       *)
      fun address(ea, mem) = 
      let (* tricky way to negate without overflow! *)
          fun neg32 w = Word32.notb w + 0w1

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
                 T.LI n   => doEAImmed(trees, n, b, i, s, d)
               | T.LI32 n => doEAImmedw(trees, n, b, i, s, d)
               | T.CONST c => doEALabel(trees, LE.CONST c, b, i, s, d)
               | T.LABEL le => doEALabel(trees, le, b, i, s, d)
               | T.ADD(32, t1, t2 as T.REG(_,r)) => 
                    if isMemReg r then doEA(t2::t1::trees, b, i, s, d)
                    else doEA(t1::t2::trees, b, i, s, d)
               | T.ADD(32, t1, t2) => doEA(t1::t2::trees, b, i, s, d)
               | T.SUB(32, t1, T.LI n) => 
                    (* can't overflow here *)
                    doEA(t1::T.LI32(neg32(Word32.fromInt n))::trees, b, i, s, d)
               | T.SUB(32, t1, T.LI32 n) => 
                    doEA(t1::T.LI32(neg32 n)::trees, b, i, s, d)
               | T.SLL(32, t1, T.LI 0) => displace(trees, t1, b, i, s, d)
               | T.SLL(32, t1, T.LI 1) => indexed(trees, t1, t, 1, b, i, s, d)
               | T.SLL(32, t1, T.LI 2) => indexed(trees, t1, t, 2, b, i, s, d)
               | T.SLL(32, t1, T.LI 3) => indexed(trees, t1, t, 3, b, i, s, d)
               | T.SLL(32, t1, T.LI32 0w0) => displace(trees, t1, b, i, s, d)
               | T.SLL(32, t1, T.LI32 0w1) => indexed(trees,t1,t,1,b,i,s,d)
               | T.SLL(32, t1, T.LI32 0w2) => indexed(trees,t1,t,2,b,i,s,d)
               | T.SLL(32, t1, T.LI32 0w3) => indexed(trees,t1,t,3,b,i,s,d)
               | t => displace(trees, t, b, i, s, d)
              ) 

          (* Add an immed constant *)
          and doEAImmed(trees, 0, b, i, s, d) = doEA(trees, b, i, s, d)
            | doEAImmed(trees, n, b, i, s, I.Immed m) = 
                 doEA(trees, b, i, s, (* no overflow! *)
                       I.Immed(w32toi32(Word32.fromInt n + i32tow32 m)))
            | doEAImmed(trees, n, b, i, s, I.ImmedLabel le) = 
                 doEA(trees, b, i, s, I.ImmedLabel(LE.PLUS(le,LE.INT n)))
            | doEAImmed(trees, n, b, i, s, _) = error "doEAImmed"

          (* Add an immed32 constant *)
          and doEAImmedw(trees, 0w0, b, i, s, d) = doEA(trees, b, i, s, d)
            | doEAImmedw(trees, n, b, i, s, I.Immed m) = 
                 (* no overflow! *)
                 doEA(trees, b, i, s, I.Immed(w32toi32(i32tow32 m + n)))
            | doEAImmedw(trees, n, b, i, s, I.ImmedLabel le) = 
                 doEA(trees, b, i, s, 
                      I.ImmedLabel(LE.PLUS(le,LE.INT(Word32.toIntX n)))
                      handle Overflow => error "doEAImmedw: constant too large")
            | doEAImmedw(trees, n, b, i, s, _) = error "doEAImmedw"

          (* Add a label expression *)
          and doEALabel(trees, le, b, i, s, I.Immed 0) = 
                 doEA(trees, b, i, s, I.ImmedLabel le)
            | doEALabel(trees, le, b, i, s, I.Immed m) = 
                 doEA(trees, b, i, s, 
                      I.ImmedLabel(LE.PLUS(le,LE.INT(Int32.toInt m)))
                      handle Overflow => error "doEALabel: constant too large")
            | doEALabel(trees, le, b, i, s, I.ImmedLabel le') = 
                 doEA(trees, b, i, s, I.ImmedLabel(LE.PLUS(le,le')))
            | doEALabel(trees, le, b, i, s, _) = error "doEALabel"

          and makeAddressingMode(NONE, NONE, _, disp) = disp
            | makeAddressingMode(SOME base, NONE, _, disp) = 
                I.Displace{base=base, disp=disp, mem=mem}
            | makeAddressingMode(base, SOME index, scale, disp) = 
                I.Indexed{base=base, index=index, scale=scale, 
                          disp=disp, mem=mem}

          (* generate code for tree and ensure that it is not in %esp *)
          and exprNotEsp tree =
              let val r = expr tree
              in  if r = C.esp then 
                     let val tmp = newReg()
                     in  move(I.Direct r, I.Direct tmp); tmp end
                  else r
              end

          (* Add a base register *)
          and displace(trees, t, NONE, i, s, d) =  (* no base yet *)
               doEA(trees, SOME(expr t), i, s, d)
            | displace(trees, t, b as SOME base, NONE, _, d) = (* no index *)
              (* make t the index, but make sure that it is not %esp! *)
              let val i = expr t
              in  if i = C.esp then
                    (* swap base and index *)
                    if base <> C.esp then
                       doEA(trees, SOME i, b, 0, d)
                    else  (* base and index = %esp! *)
                       let val index = newReg()
                       in  move(I.Direct i, I.Direct index);
                           doEA(trees, b, SOME index, 0, d)
                       end
                  else
                    doEA(trees, b, SOME i, 0, d)
              end
            | displace(trees, t, SOME base, i, s, d) = (* base and index *) 
              let val b = expr(T.ADD(32,T.REG(32,base),t))
              in  doEA(trees, SOME b, i, s, d) end

          (* Add an indexed register *)
          and indexed(trees, t, t0, scale, b, NONE, _, d) = (* no index yet *)
               doEA(trees, b, SOME(exprNotEsp t), scale, d)
            | indexed(trees, _, t0, _, NONE, i, s, d) = (* no base *)
               doEA(trees, SOME(expr t0), i, s, d)
            | indexed(trees, _, t0, _, SOME base, i, s, d) = (*base and index*)
               let val b = expr(T.ADD(32, t0, T.REG(32, base)))
               in  doEA(trees, SOME b, i, s, d) end
                  
      in  case doEA([ea], NONE, NONE, 0, I.Immed 0) of 
            I.Immed _ => raise EA
          | I.ImmedLabel le => I.LabelEA le
          | ea => ea
      end (* address *)

          (* reduce an expression into an operand *)
      and operand(T.LI i) = I.Immed(toInt32 i)
        | operand(T.LI32 w) = I.Immed(wToInt32 w)
        | operand(T.CONST c) = I.ImmedLabel(LE.CONST c)
        | operand(T.LABEL lab) = I.ImmedLabel lab
        | operand(T.REG(_,r)) = IntReg r
        | operand(T.LOAD(32,ea,mem)) = address(ea, mem)
        | operand(t) = I.Direct(expr t)

      and moveToReg(opnd) =
          let val dst = I.Direct(newReg())
          in  move(opnd, dst); dst
          end

      and reduceOpnd(I.Direct r) = r
        | reduceOpnd opnd =
          let val dst = newReg()
          in  move(opnd, I.Direct dst); dst
          end

      (* ensure that the operand is either an immed or register *)
      and immedOrReg(opnd as I.Displace _) = moveToReg opnd
        | immedOrReg(opnd as I.Indexed _)  = moveToReg opnd
        | immedOrReg(opnd as I.MemReg _)   = moveToReg opnd
        | immedOrReg(opnd as I.LabelEA _)  = moveToReg opnd
        | immedOrReg opnd  = opnd

      and isImmediate(I.Immed _) = true
        | isImmediate(I.ImmedLabel _) = true
        | isImmediate _ = false

      and regOrMem opnd = if isImmediate opnd then moveToReg opnd else opnd
 
      and isMemOpnd opnd = 
          (case opnd of 
            I.Displace _ => true
          | I.Indexed _  => true 
          | I.MemReg _   => true 
          | I.LabelEA _  => true 
          | I.FDirect f  => true
          | _            => false
          )
      
         (* 
          * Compute an integer expression and put the result in 
          * the destination register rd.  
          *)
      and doExpr(exp, rd : I.C.cell, an) = 
          let val rdOpnd = IntReg rd

              fun equalRd(I.Direct r) = r = rd
                | equalRd(I.MemReg r) = r = rd
                | equalRd _ = false

                 (* Emit a binary operator.  If the destination is
                  * a memReg, do something smarter.
                  *)
              fun genBinary(binOp, opnd1, opnd2) =
                  if isMemReg rd andalso
                     (isMemOpnd opnd1 orelse isMemOpnd opnd2) orelse
                     equalRd(opnd2)
                  then
                  let val tmpR = newReg()
                      val tmp  = I.Direct tmpR
                  in  move(opnd1, tmp);
                      mark(I.BINARY{binOp=binOp, src=opnd2, dst=tmp}, an);
                      move(tmp, rdOpnd)
                  end 
                  else
                     (move(opnd1, rdOpnd);
                      mark(I.BINARY{binOp=binOp, src=opnd2, dst=rdOpnd}, an)
                     )

                 (* Generate a binary operator; it may commute *)
              fun binaryComm(binOp, e1, e2) = 
              let val (opnd1, opnd2) = 
                      case (operand e1, operand e2) of
                        (x as I.Immed _, y)      => (y, x)
                      | (x as I.ImmedLabel _, y) => (y, x)
                      | (x, y as I.Direct _)     => (y, x)
                      | (x, y)                   => (x, y)
              in  genBinary(binOp, opnd1, opnd2)
              end
        
                 (* Generate a binary operator; non-commutative *)
              fun binary(binOp, e1, e2) =
                  genBinary(binOp, operand e1, operand e2)
        
                 (* Generate a unary operator *)
              fun unary(unOp, e) = 
              let val opnd = operand e
              in  if isMemReg rd andalso isMemOpnd opnd then
                     let val tmp = I.Direct(newReg())
                     in  move(opnd, tmp); move(tmp, rdOpnd)
                     end 
                  else move(opnd, rdOpnd);
                  mark(I.UNARY{unOp=unOp, opnd=rdOpnd}, an)
              end
        
                 (* Generate shifts; the shift 
                  * amount must be a constant or in %ecx *)
              fun shift(opcode, e1, e2) =
              let val (opnd1, opnd2) = (operand e1, operand e2)
              in  case opnd2 of 
                    I.Immed _ => genBinary(opcode, opnd1, opnd2)
                  | _ => 
                    if equalRd(opnd2) then 
                    let val tmpR = newReg()
                        val tmp  = I.Direct tmpR
                    in  move(opnd1, tmp);
                        move(opnd2, ecx);
                        mark(I.BINARY{binOp=opcode, src=ecx, dst=tmp},an);
                        move(tmp, rdOpnd)
                    end
                    else
                        (move(opnd1, rdOpnd);
                         move(opnd2, ecx);
                         mark(I.BINARY{binOp=opcode, src=ecx, dst=rdOpnd},an)
                        )
              end
        
                  (* Division or remainder: divisor must be in %edx:%eax pair *)
              fun divrem(signed, overflow, e1, e2, resultReg) =
              let val (opnd1, opnd2) = (operand e1, operand e2)
                  val _ = move(opnd1, eax)
                  val oper = if signed then (emit(I.CDQ); I.IDIV)
                             else (zero edx; I.UDIV)
              in  mark(I.MULTDIV{multDivOp=oper, src=regOrMem opnd2},an);
                  move(resultReg, rdOpnd);
                  if overflow then trap() else ()
              end
       
                  (* Optimize the special case for division *) 
              fun divide(signed, overflow, e1, e2 as T.LI n) = 
              let fun isPowerOf2 w = Word.andb((w - 0w1), w) = 0w0 
                  fun log2 n =  (* n must be > 0!!! *)
                      let fun loop(0w1,pow) = pow
                            | loop(w,pow) = loop(Word.>>(w, 0w1),pow+1)
                      in loop(n,0) end
                  val w = Word.fromInt n
              in  if n > 1 andalso isPowerOf2 w then 
                     let val pow = T.LI(log2 w)
                     in  if signed then 
                         (* signed; simulate round towards zero *)
                         let val label = Label.newLabel ""
                             val reg1  = expr e1
                             val opnd1 = I.Direct reg1
                         in  if setZeroBit e1 then ()
                             else emit(I.CMPL{lsrc=opnd1, rsrc=I.Immed 0});
                             emit(I.JCC{cond=I.GE, opnd=immedLabel label});
                             emit(if n = 2 then
                                     I.UNARY{unOp=I.INCL, opnd=opnd1}
                                  else
                                     I.BINARY{binOp=I.ADDL, 
                                              src=I.Immed(toInt32 n - 1),
                                              dst=opnd1});
                             defineLabel label;
                             shift(I.SARL, T.REG(32, reg1), pow)
                         end
                         else (* unsigned *)
                            shift(I.SHRL, e1, pow)
                     end
                  else
                       (* note the only way we can overflow is if
                        * n = 0 or n = -1
                        *)
                     divrem(signed, overflow andalso (n = ~1 orelse n = 0), 
                            e1, e2, eax)
              end
                | divide(signed, overflow, e1, e2) = 
                    divrem(signed, overflow, e1, e2, eax)

              fun rem(signed, overflow, e1, e2) = 
                    divrem(signed, overflow, e1, e2, edx)
        
                  (* unsigned integer multiplication *)
              fun uMultiply(e1, e2) = 
                  (* note e2 can never be (I.Direct edx) *)
                  (move(operand e1, eax);
                   mark(I.MULTDIV{multDivOp=I.UMUL, 
                                  src=regOrMem(operand e2)},an);
                   move(eax, rdOpnd)
                  )
        
                  (* signed integer multiplication: 
                   * The only forms that are allowed that also sets the 
                   * OF and CF flags are:
                   *
                   *      imul r32, r32/m32, imm8
                   *      imul r32, imm8
                   *      imul r32, imm32
                   *)
              fun multiply(e1, e2) = 
              let fun doit(i1 as I.Immed _, i2 as I.Immed _, dstR, dst) =
                      (move(i1, dst);
                       mark(I.MUL3{dst=dstR, src1=i2, src2=NONE},an))
                    | doit(rm, i2 as I.Immed _, dstR, dst) = 
                        doit(i2, rm, dstR, dst)
                    | doit(imm as I.Immed(i), rm, dstR, dst) =
                       mark(I.MUL3{dst=dstR, src1=rm, src2=SOME i},an)
                    | doit(r1 as I.Direct _, r2 as I.Direct _, dstR, dst) =
                      (move(r1, dst);
                       mark(I.MUL3{dst=dstR, src1=r2, src2=NONE},an))
                    | doit(r1 as I.Direct _, rm, dstR, dst) =
                      (move(r1, dst);
                       mark(I.MUL3{dst=dstR, src1=rm, src2=NONE},an))
                    | doit(rm, r as I.Direct _, dstR, dst) = 
                       doit(r, rm, dstR, dst)
                    | doit(rm1, rm2, dstR, dst) =
                       if equalRd rm2 then
                       let val tmpR = newReg()
                           val tmp  = I.Direct tmpR
                       in move(rm1, tmp);
                          mark(I.MUL3{dst=tmpR, src1=rm2, src2=NONE},an);
                          move(tmp, dst)
                       end
                       else
                         (move(rm1, dst);
                          mark(I.MUL3{dst=dstR, src1=rm2, src2=NONE},an)
                         )
                  val (opnd1, opnd2) = (operand e1, operand e2)
              in  if isMemReg rd then (* destination must be a real reg *)
                  let val tmpR = newReg()
                      val tmp  = I.Direct tmpR
                  in  doit(opnd1, opnd2, tmpR, tmp); 
                      move(tmp, rdOpnd)
                  end
                  else
                      doit(opnd1, opnd2, rd, rdOpnd) 
              end

                 (* Makes sure the destination must be a register *)
              fun dstMustBeReg f = 
                  if isMemReg rd then
                  let val tmpR = newReg()
                      val tmp  = I.Direct(tmpR)
                  in  f(tmpR, tmp); move(tmp, rdOpnd) end
                  else f(rd, rdOpnd)

                 (* Emit a load instruction; makes sure that the destination
                  * is a register 
                  *)
              fun genLoad(mvOp, ea, mem) = 
                  dstMustBeReg(fn (_, dst) =>
                     mark(I.MOVE{mvOp=mvOp, src=address(ea, mem), dst=dst},an))
    
                 (* Generate a zero extended loads *)
              fun load8(ea, mem) = genLoad(I.MOVZBL, ea, mem)
              fun load16(ea, mem) = genLoad(I.MOVZWL, ea, mem)
              fun load8s(ea, mem) = genLoad(I.MOVSBL, ea, mem)
              fun load16s(ea, mem) = genLoad(I.MOVSWL, ea, mem)
              fun load32(ea, mem) = genLoad(I.MOVL, ea, mem)
        
                 (* Generate a sign extended loads *)
        
                 (* Generate setcc instruction:
                  *  semantics:  MV(rd, COND(_, T.CMP(ty, cc, t1, t2), yes, no))
                  *)
              fun setcc(ty, cc, t1, t2, yes, no) = 
              let val tmpR = newReg()
                  val tmp = I.Direct tmpR
                  (* We create a temporary here just in 
                   * case t1 or t2 contains a use of rd.
                   *)
              in  (* Clear the destination first.
                   * This this because stupid SETcc 
                   * only writes to the low order
                   * byte.  That's Intel architecture, folks.
                   *)
                  zero tmp;
                  case (yes, no) of
                    (1, 0) => (* normal case *)
                    let val cc = cmp(true, ty, cc, t1, t2, []) 
                    in  mark(I.SET{cond=cond cc, opnd=tmp}, an) end 
                  | (0, 1) => (* flip *)
                    let val cc = cmp(true, ty, 
                                     T.Basis.negateCond cc, t1, t2, []) 
                    in  mark(I.SET{cond=cond cc, opnd=tmp}, an) end 
                  | (C1, C2)  => 
                    (* general case; 
                     * from the Intel optimization guide p3-5 *)
                    let val C1 = toInt32 C1
                        val C2 = toInt32 C2
                        val cc = cmp(true, ty, cc, t1, t2, []) 
                    in  emit(I.SET{cond=cond cc, opnd=tmp}); 
                        case Int32.abs(C1-C2)-1 of
                          D as (1 | 2 | 4 | 8) =>
                          let val addr = I.Indexed{base=SOME tmpR,
                                                   index=tmpR,
                                                   scale=Int32.toInt D,
                                                   disp=I.Immed(C1-C2),
                                                   mem=readonly}
                          in  mark(I.LEA{r32=tmpR, addr=addr}, an) end
                        | _ =>
                         (emit(I.UNARY{unOp=I.DECL, opnd=tmp});
                          emit(I.BINARY{binOp=I.ANDL,
                                        src=I.Immed(C2-C1), dst=tmp});
                          mark(I.BINARY{binOp=I.ADDL,
                                        src=I.Immed(Int32.min(C1,C2)), 
                                        dst=tmp}, an)
                         )
                    end; 
                  move(tmp, rdOpnd)
              end (* setcc *)
    
                  (* Generate cmovcc instruction.
                   * on Pentium Pro and Pentium II only
                   *)
              fun cmovcc(ty, cc, t1, t2, yes, no) = 
              let fun genCmov(dstR, _) = 
                  let val _ = doExpr(no, dstR, []) (* false branch *)
                      val cc = cmp(true, ty, cc, t1, t2, [])  (* compare *)
                  in  mark(I.CMOV{cond=cond cc, src=operand yes, dst=dstR}, an) 
                  end
              in  dstMustBeReg genCmov
              end
        
              fun unknownExp exp = doExpr(Gen.compileRexp exp, rd, an) 

                  (* Generate addition *)
              fun addition(e1, e2) =
                (dstMustBeReg(fn (dstR, _) => 
                    mark(I.LEA{r32=dstR, addr=address(exp, readonly)}, an))
                handle EA => binaryComm(I.ADDL, e1, e2))

                  (* Add n to rd *)
              fun addN n =
                mark(I.BINARY{binOp=I.ADDL, src=I.Immed(toInt32 n), 
                              dst=rdOpnd}, an)

          in  case exp of
               T.REG(_,rs) => 
                   if isMemReg rs andalso isMemReg rd then 
                      let val tmp = I.Direct(newReg())
                      in  move'(MemReg rs, tmp, an);
                          move'(tmp, rdOpnd, [])
                      end
                   else move'(IntReg rs, rdOpnd, an)
             | (T.LI 0 | T.LI32 0w0) =>  
                 (* As per Fermin's request, special optimization for rd := 0. 
                  * Currently we don't bother with the size.
                  *)
                 if isMemReg rd then move'(I.Immed 0, rdOpnd, an)
                 else mark(I.BINARY{binOp=I.XORL, src=rdOpnd, dst=rdOpnd}, an)
             | T.LI n      => move'(I.Immed(toInt32 n), rdOpnd, an)
             | T.LI32 w    => move'(I.Immed(wToInt32 w), rdOpnd, an)
             | T.CONST c   => move'(I.ImmedLabel(LE.CONST c), rdOpnd, an)
             | T.LABEL lab => move'(I.ImmedLabel lab, rdOpnd, an)

               (* 32-bit addition *)
             | T.ADD(32, e, (T.LI 1|T.LI32 0w1)) => unary(I.INCL, e)
             | T.ADD(32, (T.LI 1|T.LI32 0w1), e) => unary(I.INCL, e)
             | T.ADD(32, e, T.LI ~1) => unary(I.DECL, e)
             | T.ADD(32, T.LI ~1, e) => unary(I.DECL, e)
             | T.ADD(32, e1 as T.REG(_, rs), e2 as T.LI n) =>
                  if rs = rd then addN n else addition(e1, e2)
             | T.ADD(32, e1 as T.LI n, e2 as T.REG(_, rs)) =>
                  if rs = rd then addN n else addition(e1, e2)
             | T.ADD(32, e1, e2) => addition(e1, e2)

               (* 32-bit subtraction *)
             | T.SUB(32, e, (T.LI 1 | T.LI32 0w1)) => unary(I.DECL, e)
             | T.SUB(32, e, T.LI ~1) => unary(I.INCL, e)
             | T.SUB(32, (T.LI 0 | T.LI32 0w0), e) => unary(I.NEGL, e)

             (* Never mind:
               | T.SUB(32, e1, e2 as T.LI n) => 
                 (mark(I.LEA{r32=rd, addr=address(T.ADD(32, e1, T.LI(~n)),
                                                  I.Region.readonly)}, an)
                  handle (Overflow|EA) => binary(I.SUBL, e1, e2))
             *)      
             | T.SUB(32, e1, e2) => binary(I.SUBL, e1, e2)

             | T.MULU(32, x, y) => uMultiply(x, y)
             | T.DIVU(32, x, y) => divide(false, false, x, y)
             | T.REMU(32, x, y) => rem(false, false, x, y)

             | T.MULS(32, x, y) => multiply(x, y)
             | T.DIVS(32, x, y) => divide(true, false, x, y)
             | T.REMS(32, x, y) => rem(true, false, x, y)

             | T.ADDT(32, x, y) => (binaryComm(I.ADDL, x, y); trap())
             | T.SUBT(32, x, y) => (binary(I.SUBL, x, y); trap())
             | T.MULT(32, x, y) => (multiply(x, y); trap())
             | T.DIVT(32, x, y) => divide(true, true, x, y)
             | T.REMT(32, x, y) => rem(true, true, x, y)

             | T.ANDB(32, x, y) => binaryComm(I.ANDL, x, y)
             | T.ORB(32, x, y)  => binaryComm(I.ORL, x, y)
             | T.XORB(32, x, y) => binaryComm(I.XORL, x, y)
             | T.NOTB(32, x)    => unary(I.NOTL, x)

             | T.SRA(32, x, y)  => shift(I.SARL, x, y)
             | T.SRL(32, x, y)  => shift(I.SHRL, x, y)
             | T.SLL(32, x, y)  => shift(I.SHLL, x, y)

             | T.LOAD(8, ea, mem) => load8(ea, mem)
             | T.LOAD(16, ea, mem) => load16(ea, mem)
             | T.LOAD(32, ea, mem) => load32(ea, mem)
             | T.CVTI2I(_,T.SIGN_EXTEND,_,T.LOAD(8,ea,mem)) => load8s(ea, mem)
             | T.CVTI2I(_,T.SIGN_EXTEND,_,T.LOAD(16,ea,mem)) => load16s(ea, mem)

             | T.COND(32, T.CMP(ty, cc, t1, t2), T.LI yes, T.LI no) => 
                 setcc(ty, cc, t1, t2, yes, no)
             | T.COND(32, T.CMP(ty, cc, t1, t2), yes, no) => 
                (case !arch of (* PentiumPro and higher has CMOVcc *)
                   Pentium => unknownExp exp
                 | _ => cmovcc(ty, cc, t1, t2, yes, no)
                )
             | T.LET(s,e) => (doStmt s; doExpr(e, rd, an))
             | T.MARK(e, A.MARKREG f) => (f rd; doExpr(e, rd, an))
             | T.MARK(e, a) => doExpr(e, rd, a::an)
             | T.PRED(e,c) => doExpr(e, rd, A.CTRLUSE c::an)
             | T.REXT e => 
                 ExtensionComp.compileRext (reducer()) {e=e, rd=rd, an=an} 
               (* simplify and try again *)
             | exp => unknownExp exp
          end (* doExpr *)

          (* generate an expression and return its result register 
           * If rewritePseudo is on, the result is guaranteed to be in a 
           * non memReg register
           *)
      and expr(exp as T.REG(_, rd)) = 
          if isMemReg rd then genExpr exp else rd
        | expr exp = genExpr exp

      and genExpr exp = 
          let val rd = newReg() in doExpr(exp, rd, []); rd end

         (* Compare an expression with zero.
          * On the x86, TEST is superior to AND for doing the same thing,
          * since it doesn't need to write out the result in a register.
          *)
     and cmpWithZero(cc as (T.EQ | T.NE), e as T.ANDB(ty, a, b))  = 
            (case ty of
               8 =>  test(I.TESTB, a, b)
             | 16 => test(I.TESTW, a, b)
             | 32 => test(I.TESTL, a, b)
             | _  => (expr e; ())
             ; cc)
        | cmpWithZero(cc, e) = (expr e; cc)

          (* Emit a test.
           *   The available modes are
           *      r/m, r
           *      r/m, imm
           * On selecting the right instruction: TESTL/TESTW/TESTB.   
           * When anding an operand with a constant
           * that fits within 8 (or 16) bits, it is possible to use TESTB,
           * (or TESTW) instead of TESTL.   Because x86 is little endian, 
           * this works for memory operands too.  However, with TESTB, it is
           * not possible to use registers other than 
           * AL, CL, BL, DL, and AH, CH, BH, DH.  So, the best way is to
           * perform register allocation first, and if the operand registers
           * are one of EAX, ECX, EBX, or EDX, replace the TESTL instruction 
           * by TESTB.
           *)
      and test(testopcode, a, b) = 
          let val (_, opnd1, opnd2) = commuteComparison(T.EQ, true, a, b)
              (* translate r, r/m => r/m, r *)
              val (opnd1, opnd2) = 
                   if isMemOpnd opnd2 then (opnd2, opnd1) else (opnd1, opnd2)
          in  emit(testopcode{lsrc=opnd1, rsrc=opnd2})
          end

         (* generate a condition code expression 
           * The zero is for setting the condition code!  
           * I have no idea why this is used.
           *)
      and doCCexpr(T.CMP(ty, cc, t1, t2), 0, an) = 
          (cmp(false, ty, cc, t1, t2, an); ())
        | doCCexpr(T.CCMARK(e,A.MARKREG f),rd,an) = (f rd; doCCexpr(e,rd,an))
        | doCCexpr(T.CCMARK(e,a), rd, an) = doCCexpr(e,rd,a::an)
        | doCCexpr(T.CCEXT e, cd, an) = 
           ExtensionComp.compileCCext (reducer()) {e=e, ccd=cd, an=an} 
        | doCCexpr _ = error "doCCexpr"

     and ccExpr e = error "ccExpr"

          (* generate a comparison and sets the condition code;
           * return the actual cc used.  If the flag swapable is true,
           * we can also reorder the operands. 
           *)
      and cmp(swapable, ty, cc, t1, t2, an) = 
          (case cc of
             (T.EQ | T.NE) => 
              (* Sometimes the comparison is not necessary because
               * the bits are already set! 
               *)
              if isZero t1 andalso setZeroBit t2 then cmpWithZero(cc, t2)
              else if isZero t2 andalso setZeroBit t1 then cmpWithZero(cc, t1)
                   (* == and <> can be reordered *)
              else genCmp(ty, true, cc, t1, t2, an) 
           |  _ => genCmp(ty, swapable, cc, t1, t2, an)
          )

          (* Give a and b which are the operands to a comparison (or test)
           * Return the appropriate condition code and operands.
           *   The available modes are:
           *        r/m, imm
           *        r/m, r
           *        r,   r/m
           *)
      and commuteComparison(cc, swapable, a, b) = 
          let val (opnd1, opnd2) = (operand a, operand b)
          in  (* Try to fold in the operands whenever possible *)
              case (isImmediate opnd1, isImmediate opnd2) of
                (true, true) => (cc, moveToReg opnd1, opnd2)
              | (true, false) => 
                   if swapable then (T.Basis.swapCond cc, opnd2, opnd1)
                   else (cc, moveToReg opnd1, opnd2)
              | (false, true) => (cc, opnd1, opnd2)
              | (false, false) => 
                 (case (opnd1, opnd2) of
                    (_, I.Direct _) => (cc, opnd1, opnd2)
                  | (I.Direct _, _) => (cc, opnd1, opnd2)
                  | (_, _)          => (cc, moveToReg opnd1, opnd2)
                 )
          end 
 
          (* generate a real comparison; return the real cc used *)
      and genCmp(ty, swapable, cc, a, b, an) = 
          let val (cc, opnd1, opnd2) = commuteComparison(cc, swapable, a, b)
          in  mark(I.CMPL{lsrc=opnd1, rsrc=opnd2}, an); cc 
          end

          (* generate code for jumps *)
      and jmp(T.LABEL(lexp as LE.LABEL lab), labs, an) = 
             mark(I.JMP(I.ImmedLabel lexp, [lab]), an)
        | jmp(T.LABEL lexp, labs, an) = mark(I.JMP(I.ImmedLabel lexp, labs), an)
        | jmp(ea, labs, an)           = mark(I.JMP(operand ea, labs), an)

       (* convert mlrisc to cellset:
        *)
       and cellset mlrisc =
           let val addCCReg = C.addCell C.CC
               fun g([],acc) = acc
                 | g(T.GPR(T.REG(_,r))::regs,acc)  = g(regs,C.addReg(r,acc))
                 | g(T.FPR(T.FREG(_,f))::regs,acc) = g(regs,C.addFreg(f,acc))
                 | g(T.CCR(T.CC(_,cc))::regs,acc)  = g(regs,addCCReg(cc,acc))
                 | g(T.CCR(T.FCC(_,cc))::regs,acc)  = g(regs,addCCReg(cc,acc))
                 | g(_::regs, acc) = g(regs, acc)
           in  g(mlrisc, C.empty) end

          (* generate code for calls *)
      and call(ea, flow, def, use, mem, an) = 
          mark(I.CALL(operand ea,cellset(def),cellset(use),mem),an)

          (* generate code for integer stores *)
      and store8(ea, d, mem, an) = 
          let val src = (* movb has to use %eax as source. Stupid x86! *)
                 case immedOrReg(operand d) of
                     src as I.Direct r =>
                       if r = C.eax then src else (move(src, eax); eax)
                   | src => src
          in  mark(I.MOVE{mvOp=I.MOVB, src=src, dst=address(ea,mem)},an)
          end
      and store16(ea, d, mem, an) = error "store16"
      and store32(ea, d, mem, an) = 
            move'(immedOrReg(operand d), address(ea, mem), an)

          (* generate code for branching *)
      and branch(T.CMP(ty, cc, t1, t2), lab, an) =
           (* allow reordering of operands *)
           let val cc = cmp(true, ty, cc, t1, t2, []) 
           in  mark(I.JCC{cond=cond cc, opnd=immedLabel lab}, an) end
        | branch(T.FCMP(fty, fcc, t1, t2), lab, an) = 
           fbranch(fty, fcc, t1, t2, lab, an)
        | branch(ccexp, lab, an) =
           (doCCexpr(ccexp, 0, []);
            mark(I.JCC{cond=cond(Gen.condOf ccexp), opnd=immedLabel lab}, an)
           )

          (* generate code for floating point compare and branch *)
      and fbranch(fty, fcc, t1, t2, lab, an) = 
          let fun compare() =
              let fun ignoreOrder (T.FREG _) = true
                    | ignoreOrder (T.FLOAD _) = true
                    | ignoreOrder (T.FMARK(e,_)) = ignoreOrder e
                    | ignoreOrder _ = false
              in  if ignoreOrder t1 orelse ignoreOrder t2 then 
                       (reduceFexp(fty, t2, []); reduceFexp(fty, t1, []))
                  else (reduceFexp(fty, t1, []); reduceFexp(fty, t2, []); 
                        emit(I.FXCH{opnd=C.ST(1)}));
                  emit(I.FUCOMPP)
              end
              fun andil i = emit(I.BINARY{binOp=I.ANDL,src=I.Immed(i),dst=eax})
              fun xoril i = emit(I.BINARY{binOp=I.XORL,src=I.Immed(i),dst=eax})
              fun cmpil i = emit(I.CMPL{rsrc=I.Immed(i), lsrc=eax})
              fun j(cc, lab) = mark(I.JCC{cond=cc, opnd=immedLabel lab},an)
              fun sahf() = emit(I.SAHF)
              fun branch() =
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
                   | _      => error "fbranch"
                 (*esac*)
          in  compare(); emit I.FNSTSW; branch()
          end

      and fld(32, opnd) = I.FLDS opnd
        | fld(64, opnd) = I.FLDL opnd
        | fld(80, opnd) = I.FLDT opnd
        | fld _         = error "fld"

      and fstp(32, opnd) = I.FSTPS opnd
        | fstp(64, opnd) = I.FSTPL opnd
        | fstp(80, opnd) = I.FSTPT opnd
        | fstp _         = error "fstp"

          (* generate code for floating point stores *)
      and fstore(fty, ea, d, mem, an) = 
          (case d of
             T.FREG(fty, fs) => emit(fld(fty, I.FDirect fs))
           | _ => reduceFexp(fty, d, []);
           mark(fstp(fty, address(ea, mem)), an)
          )

      and fexpr e = error "fexpr"
          
          (* generate floating point expression and put the result in fd *)
      and doFexpr(fty, T.FREG(_, fs), fd, an) = 
            (if fs = fd then () 
             else mark(I.FCOPY{dst=[fd], src=[fs], tmp=NONE}, an)
            )
        | doFexpr(fty, T.FLOAD(fty', ea, mem), fd, an) = 
            let val ea = address(ea, mem)
            in  mark(fld(fty', ea), an); 
                emit(fstp(fty, I.FDirect fd))
            end
        | doFexpr(fty, e, fd, an) =
            (reduceFexp(fty, e, []);
             mark(fstp(fty, I.FDirect fd), an)
            )

          (* 
           * Generate floating point expression using Sethi-Ullman's scheme:
           * This function evaluates a floating point expression, 
           * and put result in %ST(0).
           *)
      and reduceFexp(fty, fexp, an)  = 
          let val ST = I.ST(C.ST 0)
              val ST1 = I.ST(C.ST 1)

              datatype su_numbers = 
                LEAF of int 
              | BINARY of int * su_numbers * su_numbers
              | UNARY of int * su_numbers
 
              datatype direction = LEFT | RIGHT

              fun label(LEAF n) = n
                | label(BINARY(n, _, _)) = n
                | label(UNARY(n, _)) = n

             (* Generate tree of sethi-ullman numbers *)
              fun suBinary(t1, t2) = 
                  let val su1 = suNumbering(t1, LEFT)
                      val su2 = suNumbering(t2, RIGHT)
                      val n1 = label su1
                      val n2 = label su2
                  in  BINARY(if n1=n2 then n1+1 else Int.max(n1, n2), su1, su2)
                  end
              
              and suUnary(t) = 
                  let val su = suNumbering(t, LEFT)
                  in  UNARY(label su, su)
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
                | suNumbering(T.CVTF2F(_,_,T.FLOAD _), _) = UNARY(1, LEAF 0)
                | suNumbering(T.CVTF2F(_,_,t), _) = suUnary t
                | suNumbering(T.FMARK(e,a),x) = suNumbering(e,x)
                | suNumbering _ = error "suNumbering"
          
              fun leafEA(T.FREG(fty, f)) = (fty, I.FDirect f)
                | leafEA(T.FLOAD(fty, ea, mem)) = (fty, address(ea, mem))
                | leafEA(T.CVTF2F(_, _, T.FLOAD(fty, ea, mem))) = 
                      (fty, address(ea, mem))
                | leafEA _ = error "leafEA"
          
              fun cvti2d(t,an) =   
              let val opnd = operand t
                  fun doMemOpnd () =
                      (emit(I.MOVE{mvOp=I.MOVL, src=opnd, dst=tempMem});
                       mark(I.FILD tempMem,an))
              in  case opnd of 
                    I.Direct _ => doMemOpnd()
                  | I.Immed _ => doMemOpnd()
                  | _ => mark(I.FILD opnd, an)
              end
          
              (* traverse expression and su-number tree *)
              fun gencode(_, LEAF 0, an) = ()
                | gencode(T.FMARK(e,a), x, an) = gencode(e, x, a::an)
                | gencode(f, LEAF 1, an) = mark(fld(leafEA f), an)
                | gencode(t, BINARY(_, su1, LEAF 0), an) = 
                  let (* optimize the common case when both operands 
                       * are equal *)
                      fun sameEA(T.FREG(t1, f1), T.FREG(t2, f2)) = 
                            t1 = t2 andalso f1 = f2 
                        | sameEA _ = false

                      fun doit(oper32, oper64, t1, t2) = 
                      let val _ = gencode(t1, su1, [])
                          val (fty, src) = leafEA t2
                      in  if sameEA(t1, t2) then 
                             mark(I.FBINARY{binOp=oper64, src=ST, dst=ST}, an)
                          else
                             let val oper = 
                                     if isMemOpnd src then
                                         case fty of
                                           32 => oper32
                                         | 64 => oper64
                                         | _  => error "gencode: binary"  
                                     else oper64
                             in mark(I.FBINARY{binOp=oper, src=src, dst=ST}, an)
                             end
                      end 
                  in
                    case t of 
                       T.FADD(_, t1, t2) => doit(I.FADDS,I.FADDL,t1,t2)
                     | T.FMUL(_, t1, t2) => doit(I.FMULS,I.FMULL,t1,t2)
                     | T.FSUB(_, t1, t2) => doit(I.FSUBS,I.FSUBL,t1,t2)
                     | T.FDIV(_, t1, t2) => doit(I.FDIVS,I.FDIVL,t1,t2)
                     | _ => error "gencode.BINARY"
                  end
                | gencode(fexp, BINARY(fty, su1, su2), an) = 
                  let fun doit(t1, t2, oper, operP, operRP) = let
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
                          emit(fstp(fty, fs));
                          gencode (t1, su1, []);
                          mark(I.FBINARY{binOp=oper, src=fs, dst=ST}, an)
                        end
                    end
                  in
                    case fexp
                    of T.FADD(_, t1, t2) => doit(t1,t2,I.FADDL,I.FADDP,I.FADDP)
                     | T.FMUL(_, t1, t2) => doit(t1,t2,I.FMULL,I.FMULP,I.FMULP)
                     | T.FSUB(_, t1, t2) => doit(t1,t2,I.FSUBL,I.FSUBP,I.FSUBRP)
                     | T.FDIV(_, t1, t2) => doit(t1,t2,I.FDIVL,I.FDIVP,I.FDIVRP)
                     | _ => error "gencode.BINARY"
                  end
                | gencode(fexp, UNARY(_, LEAF 0), an) = 
                  (case fexp
                    of T.FABS(fty, t) => 
                         (emit(fld(leafEA t)); mark(I.FUNARY(I.FABS),an))
                     | T.FNEG(fty, t) => 
                         (emit(fld(leafEA t)); mark(I.FUNARY(I.FCHS),an))
                     | T.CVTI2F(_,_,t) => cvti2d(t,an) (* XXX *)
                     | _ => error "gencode.UNARY"
                   (*esac*))
                | gencode(fexp, UNARY(_, su), an) = 
                  let fun doit(oper, t) = 
                       (gencode(t, su, []); mark(I.FUNARY(oper),an))
                  in case fexp
                     of T.FABS(_, t) => doit(I.FABS, t)
                      | T.FNEG(_, t) => doit(I.FCHS, t)
                      | T.CVTF2F(_,_,t) => gencode(t, su, an)
                      | T.CVTI2F _ => error "gencode:UNARY:cvti2f"
                      | _ => error "gencode.UNARY"
                  end
                | gencode _ = error "gencode"
            
              val labels = suNumbering(fexp, LEFT)
          in  gencode(fexp, labels, an)
          end (*reduceFexp*)
 
          (* generate code for a statement *)
      and stmt(T.MV(_, rd, e), an) = doExpr(e, rd, an)
        | stmt(T.FMV(fty, fd, e), an) = doFexpr(fty, e, fd, an) 
        | stmt(T.CCMV(ccd, e), an) = doCCexpr(e, ccd, an) 
        | stmt(T.COPY(_, dst, src), an) = copy(dst, src, an)
        | stmt(T.FCOPY(fty, dst, src), an) = fcopy(fty, dst, src, an)
        | stmt(T.JMP(ctrl, e, labs), an) = jmp(e, labs, an)
        | stmt(T.CALL(e, flow, def, use, cdef, cuse, mem), an) = 
             call(e,flow,def,use,mem,an)
        | stmt(T.RET _, an) = mark(I.RET NONE, an)
        | stmt(T.STORE(8, ea, d, mem), an) = store8(ea, d, mem, an)
        | stmt(T.STORE(16, ea, d, mem), an) = store16(ea, d, mem, an)
        | stmt(T.STORE(32, ea, d, mem), an) = store32(ea, d, mem, an)
        | stmt(T.FSTORE(fty, ea, d, mem), an) = fstore(fty, ea, d, mem, an)
        | stmt(T.BCC(ctrl, cc, lab), an) = branch(cc, lab, an)
        | stmt(T.DEFINE l, _) = defineLabel l
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
          trapLabel := NONE; beginCluster 0)
      and endCluster' a =
         (case !trapLabel
          of NONE => ()
           | SOME(_, lab) => (defineLabel lab; emit(I.INTO))
          (*esac*);
          endCluster(a)
         )

      and reducer() = 
          T.REDUCER{reduceRexp    = expr,
                    reduceFexp    = fexpr,
                    reduceCCexp   = ccExpr,
                    reduceStm     = stmt,
                    operand       = operand,
                    reduceOperand = reduceOpnd,
                    addressOf     = fn e => address(e, I.Region.memory), (*XXX*)
                    emit          = mark,
                    instrStream   = instrStream, 
                    mltreeStream  = self() 
                   }

      and self() =
          S.STREAM
          {  beginCluster= beginCluster',
             endCluster  = endCluster',
             emit        = doStmt,
             pseudoOp    = pseudoOp,
             defineLabel = defineLabel,
             entryLabel  = entryLabel,
             comment     = comment,
             annotation  = annotation,
             exitBlock   = fn mlrisc => exitBlock(cellset mlrisc),
             alias       = alias,
             phi         = phi
          }

  in  self()
  end 

end (* functor *)

end (* local *)
