(* sparcinstr.sml
 *
 * Copyright 1989 by AT&T Bell Laboratories
 *
 * AUTHOR:  John Reppy
 *	    Cornell University
 *	    Ithaca, NY 14853
 *	    jhr@cs.cornell.edu
 *
 *)

structure SparcInstr =
  struct

    val branchDelayedArch = true

    datatype 'label info = INFO of {addrOf: 'label -> int, 
				    nameOf: 'label->string}
    datatype register = REG of int
    datatype fregister = FREG of int

    datatype 'label labelexp
      = LABELexp of {      (* An offset relative to a label.  The value of a *)
	  base : 'label,   (* label expression is ((dst - base) + offset). *)
	  dst : 'label,
	  offset : int
	}

    datatype 'label operand
      = REGrand of register        (* A register value *)
      | IMrand of int              (* A small integer constant (13 bits) *)
      | LABrand of 'label labelexp (* small valued label expression (13 bits)*)
      | HIrand of 'label labelexp  (* The high 22 bits of a label expression *)
      | LOrand of 'label labelexp  (* The low 10 bits of a label expression *)


    datatype cond_code
      = CC_A | CC_E | CC_NE | CC_G | CC_GE | CC_L | CC_LE | CC_GEU | 
        CC_LEU | CC_GU | CC_LU

    datatype fcond_code 
      = FCC_A | FCC_U (* unordered *) | FCC_G | FCC_UG | FCC_L | FCC_UL
      | FCC_LG | FCC_NE | FCC_E | FCC_UE | FCC_GE | FCC_UGE | FCC_LE
      | FCC_ULE | FCC_O 

    datatype 'label instruction
      = I_nop
      | I_ld of 'label ops3                 (* ld: load word *)
      | I_ldb of 'label ops3                (* ldb: load byte (unsigned) *)
      | I_ldf of 'label fmemops             (* ldf: load floating-point register *)
      | I_st of 'label ops3                 (* st: store word *)
      | I_stb of 'label ops3                (* stb: store byte *)
      | I_stf of 'label fmemops             (* stf: store f-p register *)
      | I_sethi of ('label operand * register)     (* sethi *)
      | I_bcc of (cond_code * 'label)
      | I_fbcc of (fcond_code * 'label)
      | I_jmpl of 'label ops3               (* jmpl: jump and link *)
      | I_call2				    (* call 2; used to load PC *)
      | I_add of 'label ops3                (* add *)
      | I_addcc of 'label ops3              (* add and set condition codes *)
      | I_taddcctv of 'label ops3           (* tagged add with overflow trap *)
      | I_sub of 'label ops3
      | I_subcc of 'label ops3              (* subtract and set condition codes *)
      | I_sll of 'label ops3
      | I_srl of 'label ops3
      | I_sra of 'label ops3
      | I_and of 'label ops3
      | I_andcc of 'label ops3              (* logical and, set condition codes *)
      | I_or of 'label ops3
      | I_xor of 'label ops3
      | I_not of (register * register)
      | I_tvs                               (* tvs: trap on integer overflow *)
      | I_fadd of fops3                     (* floating-point addition *)
      | I_fsub of fops3                     (* floating-point subtraction *)
      | I_fmul of fops3                     (* floating-point multiplication *)
      | I_fdiv of fops3                     (* floating-point division *)
      | I_fneg of fops2                     (* floating-point negation *)
      | I_fabs of fops2                     (* floating-point absolute value *)
      | I_fcmp of fops2                     (* floating-point comparison *)
      | I_fmov of fops2                     (* floating-point register-register move *)
      | I_fitod of fops2		    (* convert int to double floating-point *)
      withtype 'label ops3 = (register * 'label operand * register)
      and 'label fmemops = (register * 'label operand * fregister)
      and fops3 = (fregister * fregister * fregister)
      and fops2 = (fregister * fregister)

    datatype ikind = IK_NOP | IK_JUMP | IK_INSTR

    fun instrKind (I_nop) = IK_NOP
      | instrKind (I_bcc _) = IK_JUMP
      | instrKind (I_fbcc _) = IK_JUMP
      | instrKind (I_jmpl _) = IK_JUMP
      | instrKind _ = IK_INSTR

    val nop = I_nop



  (** Span dependent instructions **
   * The implementation of these instructions depends on the value of the
   * label expressions.  The last register argument is a temporary register
   * to be used in address computations (if necessary).
   *)

    datatype 'label sdi
      = SetBaseAddr of ('label labelexp * register)
      | LoadAddr of ('label labelexp * register)
      | Load of ('label labelexp * register * register)
      | LoadF of ('label labelexp * fregister * register)

    fun isSdiBranch _ = false

    fun minSize (SetBaseAddr _) = 4
      | minSize (LoadF _) = 8
      | minSize _ = 4

    local
    (* Sizes of SDIs *)
      val sz0max = (true, 0) and sz12max = (true, 12)
      val sz16max = (true, 16) and sz20max = (true, 20)
      val sz4 = (false, 4) and sz8 = (false, 8) and sz12 = (false, 12)
    in

  (* Return the size of the various span-dependent instructions.  This should
   * be consistent with expand in "sparccoder.sml" *)
    fun sizeOf (INFO{addrOf,...}) (I,_) = let
	  fun span (LABELexp{base, dst, offset}) =
		((addrOf dst) + offset) - (addrOf base)
	  fun sizeOf' labexp = let
		val x = span labexp
		in
		  if (x < ~4096) orelse (4095 < x) then sz12max else sz4
		end
	  in
	    case I
	     of SetBaseAddr(labexp,_) => let
		  val x = span labexp
		  in if (4095 < x)
		    then if (8190 < x)
		      then sz12max
		      else sz8  (* use two subtract immediates *)
		    else if (x < ~4096)
		      then sz12max
		      else sz4
		  end
	      | LoadAddr(labexp, _) => sizeOf' labexp
	      | Load(labexp, _, _) => sizeOf' labexp
	      | LoadF(labexp, _, _) => let
		  val x = span labexp
		  in
		    if (x < ~4092) orelse (4092 <= x) then sz20max else sz8
		  end
	  end

    end (* local *)

    local
      val linkReg = REG 15            (* %o7 *)
      val baseReg = REG 27            (* %i3 *)
      val zeroR = REG 0               (* %g0 *)
      val zeroRand = REGrand zeroR
    in

  (* expand SDIs into real instruction sequences. *)
    fun expand (INFO{addrOf,...}) =
     fn (SetBaseAddr(labexp,reg), 4,_) => [
	  I_sub(reg, LABrand labexp, baseReg)]
      | (SetBaseAddr(LABELexp{base, dst, offset},reg), 8,_) => let
	  val n = if (((addrOf dst) - (addrOf base)) < 0) then ~4096 else 4095
	  val labexp = LABELexp{base=base, dst=dst, offset=offset - n}
	  in [
	    I_sub(reg, IMrand n, baseReg),
	    I_sub(baseReg, LABrand labexp, baseReg)]
	  end
      | (SetBaseAddr(labexp,reg), 12,_) => [
	    I_sethi(HIrand labexp, baseReg),
	    I_or(baseReg, LOrand labexp, baseReg),
	    I_sub(reg, REGrand baseReg, baseReg)]
      | (LoadAddr(labexp, dst), 4,_) => [I_add(baseReg, LABrand(labexp), dst)]
      | (LoadAddr(labexp, dst), 12,_) => [
	  I_sethi(HIrand labexp, dst),
	  I_or(dst, LOrand labexp, dst),
	  I_add(baseReg, REGrand dst, dst)]
      | (Load(labexp, dst, _), 4,_) => [I_ld(baseReg, LABrand(labexp), dst)]
      | (Load(labexp, dst, tmpR), 12,_) => [
	  I_sethi(HIrand(labexp), tmpR),
	  I_or(tmpR, LOrand labexp, tmpR),
	  I_ld(baseReg, REGrand tmpR, dst)]
      | (LoadF(labexp as LABELexp{base, dst, offset}, FREG i, _), 8,_) => [
	  I_ldf(baseReg, LABrand(labexp), FREG i),
	  I_ldf(baseReg, LABrand(LABELexp{base=base, dst=dst, offset=offset+4}),
	    FREG(i+1))]
      | (LoadF(labexp, FREG i, tmpR), 20,_) => [
	  I_sethi(HIrand(labexp), tmpR),
	  I_or(tmpR, LOrand(labexp), tmpR),
	  I_add(baseReg, REGrand tmpR, tmpR),
	  I_ldf(tmpR, zeroRand, FREG i),
	  I_ldf(tmpR, IMrand 4, FREG(i+1)) ]

    end (* local *)


  (** Resource usage **
   *
   * The sparc resources are the condition codes, floating-point condition codes,
   * registers %r1-%r31, floating-point registers (viewed as a single resource),
   * the npc (next pc) and  memory.  We treat %g6 (the dataptr) specially.  We
   * assume that %g6 is only used in 'add', 'taddcctv', 'st', 'stf', and 'or' (move)
   * instructions.  We use %g6 to denote the "allocation" resource.  This interferes
   * in a non-standard way with the memory resource and with itself.  Store
   * instructions using %g6 as a base register don't define the memory resource, but
   * the move of %g6 (using 'or') to another register is an implicit definition of
   * the memory resource (since it makes the allocation visible), the move also
   * defines the allocation resource, since it should only occur after the record
   * has been initialized.  There is an implicit use dependency between the
   * 'tvs' instruction and the exnptr register.  The npc resource is used to create
   * a use-def dependency between trap instructions ('tvs' and 'taddcctv') and branch
   * instructions.  This avoids the having traps scheduled in branch shadows.
   *)

    val numRegs = 31        (* %r1-%r31 *)

    val numResources =      (* registers + fp regs + mem, condition codes, *)
	  (numRegs + 6)     (* fp condition codes, npc and the stack *)

    local
      val memRId = 0
      val memR = [memRId]
      fun addRegR (REG 0, lst) = lst
	| addRegR (REG i, lst) = (i :: lst)
      fun regR r = addRegR(r, nil)
      val allocRId = 6 and allocR = [6]	    (* %g6 *)
      val exnptrRId = 7			    (* %g7 *)
      val linkRId = 15 and linkR = [15]	    (* %o7  (link and base register) *)
      val fregsRId = (numRegs + 1)
      val ccRId = (fregsRId + 1)
      val fccRId = (ccRId + 1)
      val fregsR = [fregsRId]
      val ccR = [ccRId] and fccR = [fccRId]
      val npcRId = (fccRId + 1)
      val npcR = [npcRId]
      val stackRId = (npcRId + 1)
      val stackR = [stackRId] 
      fun rUseDef3 (a, REGrand b, c) = (addRegR(a, regR b), regR c)
	| rUseDef3 (a, _, c) = (regR a, regR c)
      fun rUseDef3cc (a, REGrand b, c) = (addRegR(a, regR b), ccRId :: (regR c))
	| rUseDef3cc (a, _, c) = (regR a, ccRId :: (regR c))
      val fregUseDef = (fregsR, fregsR)
      val farithUseDef = (fregsR, fregsR)
      val allR = let
	    fun f (~1, l) = l | f (i, l) = f(i-1, i::l)
	    in
	      f (numResources-1, nil)
	    end
    in

    fun rUseDef (I_nop) = ([], [])
      | rUseDef (I_ld args) = let val (u, d) = rUseDef3 args in (memRId :: u, d) end
      | rUseDef (I_ldb args) = let val (u, d) = rUseDef3 args in (memRId :: u, d) end
      | rUseDef (I_ldf(a as REG 14,_,c)) = (stackRId::regR a,fregsR)
      | rUseDef (I_ldf(a, REGrand b, c)) = (memRId :: addRegR(a, regR b), fregsR)
      | rUseDef (I_ldf(a, _, c)) = (memRId :: regR a, fregsR)
      | rUseDef (I_st(REG 6, REGrand b, c)) = (allocRId :: addRegR(b, regR c), [])
      | rUseDef (I_st(REG 6, _, c)) = (allocRId :: (regR c), [])
      | rUseDef (I_st(a as REG 14,_,c)) = (addRegR(a,regR c), stackR)
      | rUseDef (I_st(a, REGrand b, c)) = (addRegR(a, addRegR(b, regR c)), memR)
      | rUseDef (I_st(a, _, c)) = (addRegR(a, regR c), memR)
      | rUseDef (I_stb(a, REGrand b, c)) = (addRegR(a, addRegR(b, regR c)), memR)
      | rUseDef (I_stb(a, _, c)) = (addRegR(a, regR c), memR)
      | rUseDef (I_stf(REG 6, REGrand b, c)) = (allocRId :: addRegR(b, fregsR), [])
      | rUseDef (I_stf(REG 6, _, c)) = (allocRId :: fregsR, [])
      | rUseDef (I_stf(a, REGrand b, c)) = (addRegR(a, addRegR(b, fregsR)), memR)
      | rUseDef (I_stf(a, _, c)) = (addRegR(a, fregsR), memR)
      | rUseDef (I_sethi(_, r)) = ([], regR r)
      | rUseDef (I_bcc(CC_A, _)) = ([], npcR)
      | rUseDef (I_bcc _) = (ccR, npcR)
      | rUseDef (I_fbcc _) = (fccR, npcR)
      | rUseDef (I_jmpl(a, REGrand b, c)) = (addRegR(a, regR b), addRegR(c, npcR))
      | rUseDef (I_jmpl(a, _, c)) = (regR a, addRegR(c, npcR))
      | rUseDef (I_call2) = (allR, allR) (* to prevent it from being moved *)
      | rUseDef (I_add(REG 6, _, REG c)) = (* this completes the allocation *)
	  (allocR, [allocRId, c, memRId])
      | rUseDef (I_add args) = rUseDef3 args
      | rUseDef (I_addcc args) = rUseDef3cc args
      | rUseDef (I_taddcctv _) = (allR, allR) (* GC limit check *)
      | rUseDef (I_sub args) = rUseDef3 args
      | rUseDef (I_subcc args) = rUseDef3cc args
      | rUseDef (I_sll args) = rUseDef3 args
      | rUseDef (I_sra args) = rUseDef3 args
      | rUseDef (I_srl args) = rUseDef3 args
      | rUseDef (I_and args) = rUseDef3 args
      | rUseDef (I_andcc args) = rUseDef3cc args
      | rUseDef (I_or args) = rUseDef3 args
      | rUseDef (I_xor args) = rUseDef3 args
      | rUseDef (I_not(a, b)) = (regR a, regR b)
      | rUseDef (I_tvs) = ([memRId, ccRId, exnptrRId, npcRId, allocRId], [])
      | rUseDef (I_fadd _) = farithUseDef
      | rUseDef (I_fsub _) = farithUseDef
      | rUseDef (I_fmul _) = farithUseDef
      | rUseDef (I_fdiv _) = farithUseDef
      | rUseDef (I_fneg _) = fregUseDef
      | rUseDef (I_fabs _) = fregUseDef
      | rUseDef (I_fcmp _) = (fregsR, fccR)
      | rUseDef (I_fmov _) = fregUseDef
      | rUseDef (I_fitod _) = fregUseDef
    end (* local *)

    fun mayNeedNop (I_fcmp _) = 1
      | mayNeedNop (I_bcc _)  = 1
      | mayNeedNop (I_fbcc _) = 1
      | mayNeedNop (I_jmpl _) = 1
      | mayNeedNop _ 	      = 0

    fun needsNop (I_fbcc _, I_fcmp _::_) = 1
      | needsNop _ 			 = 0

  (* These numbers are true for the Fujitsu implementation *)
    fun latency (I_ld _) 	= 2
      | latency (I_ldb _) 	= 2
      | latency (I_jmpl _)	= 2
      | latency (I_taddcctv _)  = 2
      | latency I_tvs		= 2
      | latency (I_ldf _)	= 2
      | latency (I_bcc _)	= 2
      | latency (I_fbcc _)	= 2
    (* guessing on these ... *)
      | latency (I_fcmp _)	= 2
      | latency (I_fadd _)	= 3
      | latency (I_fsub _)	= 3
      | latency (I_fmul _)	= 3
      | latency (I_fdiv _)	= 4
      | latency _		= 1

  end (* SparcInstr *)

(*
 * $Log: sparcinstr.sml,v $
 * Revision 1.3  1997/08/19 14:47:01  george
 *    Bug fix for 1222 (ref assignment not performed before overflow).
 *
 * Revision 1.2  1997/05/05  19:57:35  george
 *   Add the allocation pointer to the list of source registers for the
 *   tvs instruction. This fixed the subtle bug on interactions between
 *   the allocation-pointer-adjustment instruction and the addi
 *   instruction. -- zsh
 *
 * Revision 1.1.1.1  1997/01/14  01:38:46  george
 *   Version 109.24
 *
 *)
