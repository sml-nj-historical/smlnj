(* X86Spill.sml 
 *
 * X86 spilling is complicated business. 
 *)
signature X86SPILL = sig
  structure I : X86INSTR
  val spill :  
    I.instruction * int * I.operand -> 
      {code:I.instruction list, proh:int list, instr:I.instruction option}

  val reload : 
    I.instruction * int * I.operand -> {code:I.instruction list, proh:int list}

  val fspill :  
    I.instruction * int * I.operand -> 
      {code:I.instruction list, proh:int list, instr:I.instruction option}

  val freload : 
    I.instruction * int * I.operand -> {code:I.instruction list, proh:int list}
end


functor X86Spill(structure Instr: X86INSTR
		 structure Asm : INSTRUCTION_EMITTER where I = Instr (* XXX *)
		   
		 ) : X86SPILL = struct

  (* XXX: Looks like the wrong thing is done with CMPL. 
   * That is to say, does not recognize memory arguments.
   *)
  structure I = Instr
  structure C = I.C

  fun error msg = MLRiscErrorMsg.impossible("X86Spill: "^ msg)

  type s = I.instruction * int * I.operand -> 
	     {code:I.instruction list, proh:int list, instr:I.instruction option}
  type r = I.instruction * int * I.operand -> {code:I.instruction list, proh:int list}

  val newReg = C.newReg

  fun immed(I.Immed _) = true
    | immed(I.ImmedLabel _) = true
    | immed(I.Const _) = true
    | immed(I.LabelEA _) = true
    | immed _ = false

  fun immedOrReg(I.Direct r) = true
    | immedOrReg opnd = immed opnd

  fun eqEA(I.Displace{base=b1, disp=d1,...},I.Displace{base=b2, disp=d2,...}) = 
      b1=b2 andalso
       (case (d1, d2) 
	 of (I.Immed i1, I.Immed i2) => i1 = i2
          | _ => false
       (*esac*))
    | eqEA _ = false

  (* XXX:: Need to go through all the cases where 'done' is used
   * to make sure that a the src cannot contain the register
   * being spilled.
   *)
  fun spill(instr, reg, spillLoc) = let
    fun done code = {code=code, proh=[], instr=NONE}
    fun rewrite new old = if old=reg then new else old
    fun spillIt instr =
    case instr
    of I.CALL(opnd, (r,f,c), uses, mem) => let
         val tmpR = newReg()
       in
	 {proh=[tmpR],
	  instr=SOME(I.CALL(opnd, (map (rewrite tmpR) r,f,c), uses, mem)),
	  code=[I.MOVE{mvOp=I.MOVL, src=I.Direct tmpR, dst=spillLoc}] }
       end
     | I.MOVE{mvOp=I.MOVZX, src, dst} => let(* dst must always be a register *)
         val tmpR = newReg()
       in 
	 {proh=[tmpR],
	  instr=SOME(I.MOVE{mvOp=I.MOVZX, src=src, dst=I.Direct tmpR}),
	  code=[I.MOVE{mvOp=I.MOVL, src=I.Direct tmpR, dst=spillLoc}]}
       end
     | I.MOVE{mvOp, src as I.Direct rs, dst} =>
        if rs=reg then {code=[], proh=[], instr=NONE}
	else done [I.MOVE{mvOp=mvOp, src=src, dst=spillLoc}]
     | I.MOVE{mvOp, src, dst=I.Direct _} => 
	if immed src then done [I.MOVE{mvOp=mvOp, src=src, dst=spillLoc}]
	else if eqEA(src, spillLoc) then {code=[], proh=[], instr=NONE}
	else let 
	    val tmpR = newReg()
	  in
	    {instr=SOME(I.MOVE{mvOp=mvOp, src=src, dst=I.Direct tmpR}),
	     proh=[tmpR],
	     code=[I.MOVE{mvOp=mvOp, src=I.Direct tmpR, dst=spillLoc}]}
	  end
     | I.LEA{addr, r32} => let
	 val tmpR = newReg()
       in {instr=SOME(I.LEA{addr=addr, r32=tmpR}),
	   proh=[tmpR],
	   code=[I.MOVE{mvOp=I.MOVL, src=I.Direct tmpR, dst=spillLoc}]}
       end
     | I.BINARY{binOp, src, dst} => 
       if immedOrReg src then 
	 {instr=SOME(I.BINARY{binOp=binOp, src=src, dst=spillLoc}),
	  proh=[],
	  code=[]}
       else let 
	   val tmpR = newReg()
	 in
	   {instr=SOME(I.MOVE{mvOp=I.MOVL, src=src, dst=I.Direct tmpR}),
	    proh=[tmpR],
	    code=[I.BINARY{binOp=binOp, src=I.Direct tmpR, dst=spillLoc}]}
	 end
     | I.MULTDIV _ => error "spill: MULTDIV"
     | I.MUL3{src1, src2, dst} => let
	 val tmpR = newReg()
       in
	 {instr=SOME(I.MUL3{src1=src1, src2=src2, dst=tmpR}),
	  proh=[tmpR],
	  code=[I.MOVE{mvOp=I.MOVL, src=I.Direct tmpR, dst=spillLoc}]}
       end
     | I.UNARY{unOp, opnd} => done [I.UNARY{unOp=unOp, opnd=spillLoc}]
     | I.POP _ => done [I.POP spillLoc]
     | I.COPY _ => error "spill: COPY"
     | I.FNSTSW  => error "spill: FNSTSW"
     | I.ANNOTATION{i,a} => spillIt i
     | _ => error "spill"
  in spillIt instr
  end

  fun reload(instr, reg, spillLoc) = let
    fun operand(rt, opnd) =
      (case opnd
       of I.Direct r => if r=reg then I.Direct rt else opnd
        | I.Displace{base, disp, mem} => 
	   if base=reg then I.Displace{base=rt, disp=disp, mem=mem} else opnd
	| I.Indexed{base=NONE, index, scale, disp, mem=mem} => 
	   if index=reg then
	     I.Indexed{base=NONE, index=rt, scale=scale, disp=disp, mem=mem}
	   else opnd
	| I.Indexed{base as SOME b, index, scale, disp, mem=mem} => 
	   if b=reg then 
	     operand(rt, I.Indexed{base=SOME rt, index=index, 
				   scale=scale, disp=disp, mem=mem})
	   else if index=reg then
	     I.Indexed{base=base, index=rt, scale=scale, disp=disp, mem=mem}
		else opnd
	| opnd => opnd
      (*esac*))

    fun uses(I.Direct r) = r = reg	(* XXX *)
      | uses(I.Displace{base, disp, mem}) = base=reg
      | uses(I.Indexed{base=NONE, index, scale, disp, mem}) = index=reg
      | uses(I.Indexed{base=SOME b, index, scale, disp, mem})= 
          b=reg orelse index=reg
      | uses _ = false
    fun done c = {code=c, proh=[]}
    fun rewrite rt r = if r=reg then rt else r
    fun ldSpillLoc (tmpR) = I.MOVE{mvOp=I.MOVL, src=spillLoc, dst=I.Direct tmpR}
    fun withTmp f = let 
      val tmpR = newReg()
    in {code=[ldSpillLoc(tmpR), f tmpR], proh=[tmpR]}
    end
    fun reloadIt instr =
    case instr
    of I.JMP(I.Direct _, labs) => {code=[I.JMP(spillLoc, labs)], proh=[]}
     | I.JMP(opnd, labs) => withTmp(fn tmpR => I.JMP(operand(tmpR, opnd), labs))
     | I.JCC{opnd=I.Direct _, cond} => done[I.JCC{opnd=spillLoc, cond=cond}]
     | I.JCC{opnd, cond} => 
	withTmp(fn tmpR => I.JCC{opnd=operand(tmpR, opnd), cond=cond})
     | I.CALL(opnd, defs, uses as (r,f,c), mem) => 
	withTmp(fn tmpR =>
		  I.CALL(operand(tmpR, opnd), defs, 
                         (map (rewrite tmpR) r, f,c), mem))
     | I.MOVE{mvOp, src, dst as I.Direct _} => 
	withTmp(fn tmpR =>I.MOVE{mvOp=mvOp, src=operand(tmpR, src), dst=dst})
     | I.MOVE{mvOp, src as I.Direct _, dst} => 
	if eqEA(dst, spillLoc) then {code=[], proh=[]}
	else withTmp
	  (fn tmpR => 
	     I.MOVE{mvOp=mvOp, src=operand(tmpR, src), dst=operand(tmpR, dst)})
     | I.MOVE{mvOp, src, dst} => 
	withTmp
	 (fn tmpR => 
	    I.MOVE{mvOp=mvOp, src=operand(tmpR, src), dst=operand(tmpR, dst)})
     | I.LEA{r32, addr} => 
	withTmp(fn tmpR => I.LEA{r32=r32, addr=operand(tmpR, addr)})
     | I.CMP{lsrc, rsrc} => 
	withTmp(fn tmpR => I.CMP{lsrc=operand(tmpR, lsrc), rsrc=operand(tmpR, rsrc)})
     | I.BINARY{binOp, src, dst as I.Direct _} => 
	withTmp(fn tmpR => I.BINARY{binOp=binOp, src=operand(tmpR, src), dst=dst})
     | I.BINARY{binOp, src, dst} => 
	withTmp
	 (fn tmpR =>
	    I.BINARY{binOp=binOp, src=operand(tmpR, src), dst=operand(tmpR,dst)})
     | I.MULTDIV{multDivOp, src} => 
	withTmp(fn tmpR => I.MULTDIV{multDivOp=multDivOp, src=operand(tmpR, src)})
     | I.MUL3{src1, src2, dst} => 
	withTmp(fn tmpR => 
          I.MUL3{src1=operand(tmpR, src1), src2=src2, 
		 dst=if dst = reg then error "reload:MUL3" else dst})
     | I.UNARY{unOp, opnd} => 
	withTmp(fn tmpR => I.UNARY{unOp=unOp, opnd=operand(tmpR, opnd)})
     | I.PUSH arg => withTmp(fn tmpR => I.PUSH(operand(tmpR, arg)))
     | I.COPY _ => error "reload:COPY"
     | I.FILD opnd => withTmp(fn tmpR => I.FILD(operand(tmpR, opnd)))
     | I.FLD opnd => 
	withTmp (fn tmpR => I.FLD(operand(tmpR, opnd)))
     | I.FSTP opnd => 
	withTmp(fn tmpR => I.FSTP(operand(tmpR, opnd)))
     | I.FBINARY{binOp, src, dst} => 
	withTmp(fn tmpR => 
		  I.FBINARY{binOp=binOp, src=operand(tmpR, src), dst=dst})
				     
     | I.ANNOTATION{i,a} => reloadIt i
     | _ => error "reload"
  in reloadIt instr
  end (*reload*)

  fun fspill(instr, reg, spillLoc) = 
    (case instr
      of I.FSTP _ => {proh=[], instr=NONE, code=[I.FSTP spillLoc]}
       | I.ANNOTATION{i,a} => fspill(i, reg, spillLoc)
       | _ => error "fspill"
    (*esac*))

  fun freload(instr, reg, spillLoc) = 
    (case instr
      of I.FLD opnd => {code=[I.FLD spillLoc], proh=[]}
       | I.FBINARY{binOp, src=I.FDirect f, dst} => 
	   if f = reg then 
	     {code=[I.FBINARY{binOp=binOp, src=spillLoc, dst=dst}],
	      proh=[]}
	   else error "freload:FBINARY"
       | I.ANNOTATION{i,a} => freload(i, reg, spillLoc)
       | _  => error "freload"
    (*esac*))
end
