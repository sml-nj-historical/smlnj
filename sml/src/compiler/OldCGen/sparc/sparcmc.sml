(* sparcmc.sml
 *
 * Copyright 1989 by AT&T Bell Laboratories
 *
 * The SPARC machine code emitter.
 *
 * AUTHOR:  John Reppy
 *	    Cornell University
 *	    Ithaca, NY 14853
 *	    jhr@cs.cornell.edu
 *
 *)

structure SparcMCode =
struct
    local open Word8Array in

    val code = ref (array(0, 0w0))

    fun getCodeString () = let
	  val s = extract (!code, 0, SOME(length (!code)))
	  in
	    code := array(0, 0w0);
	    s
	  end

    end (* local *)
end (* SparcMCode *)

structure SparcMCEmit : EMITTER =
struct

    open SparcMCode SparcInstr

  (* Bit-wise operations *)
    val << = Word.<<
    val >> = Word.>>
    val ~>> = Word.~>>
    val ++ = Word.orb
    val & = Word.andb
    infix << >> ~>> ++ &

    val itow = Word.fromInt

    val loc = ref 0     (* the location counter *)

    fun emitByte' b = let
	  val i = !loc
	  in
	    loc := i+1;  Word8Array.update (!code, i, b)
	  end

    fun emitByte n = emitByte'(Word8.fromLargeWord(Word.toLargeWord n))

    fun emitWord w = (emitByte((w >> 0w8) & 0w255); emitByte(w & 0w255))

    fun emitLargeWord w = let
	  val w = Word.fromLargeWord w
	  in
	    emitWord(w >> 0w16); emitWord(w & 0w65535)
	  end

    fun emitLong n = let 
      val w = itow n
    in
      (emitWord(w >> 0w16); emitWord(w & 0w65535))
    end

    fun emitLongX n = let
      val w = itow n
    in
      (emitWord(w ~>> 0w16); emitWord(w & 0w65535))
    end

    fun emitString s = Word8Vector.app emitByte' (Byte.stringToBytes s)

    exception BadReal = IEEEReal.BadReal
    fun emitReal s = emitString(IEEEReal.realconst s)

    fun emitAddr (INFO{addrOf,...}) (lab, k) = 
      emitLongX (k + addrOf lab - !loc)

    fun define _ _ = ()

    fun mark () = emitLargeWord (
	  SparcSpec.ObjDesc.makeDesc(
	    (!loc + 4) div 4, SparcSpec.ObjDesc.tag_backptr))

    fun emitInstr (info as INFO{addrOf,...}) = let
	  fun valOf (LABELexp{base, dst, offset}) =
		itow(((addrOf dst) + offset) - (addrOf base))
	  fun immed13 (IMrand i) = (itow i & 0w8191)              (* 13 bits *)
	    | immed13 (LABrand labexp) = ((valOf labexp) & 0w8191)(* 13 bits *)
	    | immed13 (LOrand labexp) = ((valOf labexp) & 0w1023) (* 10 bits *)
	    | immed13 _ = ErrorMsg.impossible "[SparcMCEmit.immed13]"
	(* emit a 3 operand instruction with "11" in bits 31-30 *)
	  fun emitOp11 opcode (REG a, REGrand(REG b), REG c) = (
		emitWord(0w49152 ++ (itow c << 0w9) ++ 
			    (opcode << 0w3) ++ (itow a >> 0w2));
		emitWord(((itow a & 0w3) << 0w14) ++ itow b))
	    | emitOp11 opcode (REG a, b, REG c) = (
		emitWord(0w49152 ++ (itow c << 0w9) ++ 
			    (opcode << 0w3) ++ (itow a >> 0w2));
		emitWord(((itow a & 0w3) << 0w14) ++ 0w8192 ++ (immed13 b)))

	  val emit_ld = emitOp11 0w0
	  val emit_st = emitOp11 0w4
	  val emit_ldb = emitOp11 0w1
	  val emit_stb = emitOp11 0w5
	  fun emit_ldf (r, ri, FREG fr) = emitOp11 0w32 (r, ri, REG fr)
	  fun emit_stf (r, ri, FREG fr) = emitOp11 0w36 (r, ri, REG fr)

	(* emit a branch instruction *)
	  fun emitBcc opcode lab = let
		val disp = (((addrOf lab) - !loc) div 4)
		val d = itow(if disp < 0 then (disp + 4194304) else disp)
		in
		  if ((disp < ~2097152) orelse (2097152 <= disp))
		    then ErrorMsg.impossible "[SparcMCEmit.emitBcc]" else ();
		  let 
		    val w = opcode << 0w22 ++ d
		  in
		    emitWord(w >> 0w16);
		    emitWord(w & 0w65535)
		  end
		end

	  val emit_ba = emitBcc 0w66        (* 1000010 *)
	  val emit_be = emitBcc 0w10        (* 0001010 *)
	  val emit_bne = emitBcc 0w74       (* 1001010 *)
	  val emit_ble = emitBcc 0w18       (* 0010010 *)
	  val emit_bge = emitBcc 0w90       (* 1011010 *)
	  val emit_bl = emitBcc 0w26        (* 0011010 *)
	  val emit_bg = emitBcc 0w82        (* 1010010 *)
	  val emit_bgeu = emitBcc 0w106     (* 1101010 *)
	  val emit_bleu = emitBcc 0w34      (* 0100010 *)
	  val emit_blu = emitBcc 0wx2a      (* 0101010 *)
	  val emit_bgu = emitBcc 0wx62      (* 1100010 *)

	  val emit_fbe = emitBcc 0w78       (* 1001110 *)
	  val emit_fbne = emitBcc 0w14      (* 0001110 *)
	  val emit_fble = emitBcc 0w110     (* 1101110 *)
	  val emit_fbge = emitBcc 0w94      (* 1011110 *)
	  val emit_fbl = emitBcc 0w38       (* 0100110 *)
	  val emit_fbg = emitBcc 0w54       (* 0110110 *)
	  val emit_fblg = emitBcc 0wx16	    (* 0010110 *)
	  val emit_fba = emitBcc 0wx46	    (* 1000110 *)
	  val emit_fbu = emitBcc 0wx3e	    (* 0111110 *)	
	  val emit_fbug = emitBcc 0wx2e	    (* 0101110 *)	
	  val emit_fbul = emitBcc 0wx1e	    (* 0011110 *)	
	  val emit_fbue = emitBcc 0wx56	    (* 1010110 *)	
	  val emit_fbuge = emitBcc 0wx66    (* 1100110 *)	
	  val emit_fbule = emitBcc 0wx76    (* 1110110 *)	
	  val emit_fbo = emitBcc 0wx7e	    (* 1111110 *)	

	(* emit a 3 operand instructions with "10" in bits 31-30. *)
	  fun emitOp10 opcode (REG a, REGrand(REG b), REG c) = (
		emitWord(0w32768 ++ (itow c << 0w9) ++ 
			    (opcode << 0w3) ++ (itow a >> 0w2));
		emitWord(((itow a & 0w3) << 0w14) ++ itow b))
	    | emitOp10 opcode (REG a, b, REG c) = (
		emitWord(0w32768 ++ (itow c << 0w9) ++ 
			    (opcode << 0w3) ++ (itow a >> 0w2));
		emitWord(((itow a & 0w3) << 0w14) ++ 0w8192 ++ (immed13 b)))

	  val emit_jmpl = emitOp10 0w56       (* 111000 *)

	  fun emit_call2 () = (		(* opcode = 0x40000002 *)
		emitByte 0w64; emitByte 0w0; emitByte 0w0; emitByte 0w2)
	(* integer operations *)
	  val emit_add = emitOp10 0w0         (* 000000 *)
	  val emit_addcc = emitOp10 0w16      (* 010000 *)
	  val emit_taddcctv = emitOp10 0w34   (* 100010 *)
	  val emit_sub = emitOp10 0w4         (* 000100 *)
	  val emit_subcc = emitOp10 0w20      (* 010100 *)
	  val emit_sll = emitOp10 0w37        (* 100101 *)
	  val emit_srl = emitOp10 0w38	    (* 100110 *)
	  val emit_sra = emitOp10 0w39        (* 100111 *)
	  val emit_and = emitOp10 0w1         (* 000001 *)
	  val emit_andcc = emitOp10 0w17      (* 010001 *)
	  val emit_or = emitOp10 0w2          (* 000010 *)
	  val emit_xor = emitOp10 0w3         (* 000011 *)
	  val emit_xnor = emitOp10 0w7        (* 000111 *)

	(* emit a FOp1 floating-point instruction of three args; this has "10" in
	 * bits 31-30 and "110100" in bits 24-19.
	 *)
	  fun emitFOp1_3 opcode (FREG a, FREG b, FREG c) = (
		emitWord (0w33184 ++ (itow c << 0w9) ++ (itow a >> 0w2));
		emitWord (((itow a & 0w3) << 0w14) ++ (opcode << 0w5) ++ 
			       itow b))
	(* emit a FOp1 floating-point instruction of two args (same bits as above) *)
	  fun emitFOp1_2 opcode (FREG a, FREG b) = (
		emitWord (0w33184 ++ (itow b << 0w9));
		emitWord ((opcode << 0w5) ++ itow a))
	(* emit a FOp2 floating-point instruction of two args.  This has "10" in
	 * bits 31-30 and "110101" in bits 24-19.
	 *)
	  fun emitFOp2_2 opcode (FREG a, FREG b) = (
		emitWord (0w33192 ++ (itow a >> 0w2));
		emitWord (((itow a & 0w3) << 0w14) ++ (opcode << 0w5)
			      ++ itow b))
	  val emit_fadd  = emitFOp1_3 0wx042    (* 0 0100 0010 *)
	  val emit_fsub  = emitFOp1_3 0wx046    (* 0 0100 0110 *)
	  val emit_fmul  = emitFOp1_3 0wx04a    (* 0 0100 1010 *)
	  val emit_fdiv  = emitFOp1_3 0wx04e    (* 0 0100 1110 *)
	  val emit_fneg  = emitFOp1_2 0wx005    (* 0 0000 0101 *)
	  val emit_fabs  = emitFOp1_2 0wx009    (* 0 0000 1001 *)
	  val emit_fcmp  = emitFOp2_2 0wx052    (* 0 0101 0010 *)
	  val emit_fmov  = emitFOp1_2 0wx001    (* 0 0000 0001 *)
	  val emit_fitod = emitFOp1_2 0wx0c8    (* 0 1100 1000 *)
	  in

	    fn I_nop => emitLong 16777216 (* really "sethi 0,%g0" *)
	     | I_ld args => emit_ld args
	     | I_ldb args => emit_ldb args
	     | I_ldf args => emit_ldf args
	     | I_st args => emit_st args
	     | I_stb args => emit_stb args
	     | I_stf args => emit_stf args
	     | I_sethi(arg, REG rd) => let
		val im = case arg
		     of (IMrand i) => itow i
		      | (HIrand labexp) => ((valOf labexp) >> 0w10)
		      | _ => ErrorMsg.impossible "[SparcMCEmit.emitInstr:sethi]"
		in
		  emitWord(0w256 ++ (itow rd << 0w9) ++ ((im >> 0w16) & 0w63));
		  emitWord(im & 0w65535)
		end
	     | I_bcc(CC_A, lab) => emit_ba lab
	     | I_bcc(CC_E, lab) => emit_be lab
	     | I_bcc(CC_NE, lab) => emit_bne lab
	     | I_bcc(CC_L, lab) => emit_bl lab
	     | I_bcc(CC_LE, lab) => emit_ble lab
	     | I_bcc(CC_G, lab) => emit_bg lab
	     | I_bcc(CC_GE, lab) => emit_bge lab
	     | I_bcc(CC_GEU, lab) => emit_bgeu lab
	     | I_bcc(CC_LEU, lab) => emit_bleu lab
	     | I_bcc(CC_LU, lab)  => emit_blu lab
	     | I_bcc(CC_GU, lab) => emit_bgu lab

	     | I_fbcc(FCC_A, lab)   => emit_fba lab
	     | I_fbcc(FCC_U, lab)   => emit_fbu lab
	     | I_fbcc(FCC_G, lab)   => emit_fbg lab
	     | I_fbcc(FCC_UG, lab)  => emit_fbug lab
	     | I_fbcc(FCC_L, lab)   => emit_fbl lab 
	     | I_fbcc(FCC_UL, lab)  => emit_fbul lab
	     | I_fbcc(FCC_LG, lab)  => emit_fblg lab
	     | I_fbcc(FCC_NE, lab)  => emit_fbne lab
	     | I_fbcc(FCC_E, lab)   => emit_fbe lab
	     | I_fbcc(FCC_UE, lab)  => emit_fbue lab
	     | I_fbcc(FCC_GE, lab)  => emit_fbge lab
	     | I_fbcc(FCC_UGE, lab) => emit_fbuge lab
	     | I_fbcc(FCC_LE, lab)  => emit_fble lab
	     | I_fbcc(FCC_ULE, lab) => emit_fbule lab
	     | I_fbcc(FCC_O, lab)   => emit_fbo lab

	     | I_jmpl args => emit_jmpl args
	     | I_call2 => emit_call2()
	     | I_add args => emit_add args
	     | I_addcc args => emit_addcc args
	     | I_taddcctv args => emit_taddcctv args
	     | I_sub args => emit_sub args
	     | I_subcc args => emit_subcc args
	     | I_sll args => emit_sll args
	     | I_srl args => emit_srl args
	     | I_sra args => emit_sra args
	     | I_and args => emit_and args
	     | I_andcc args => emit_andcc args
	     | I_or args => emit_or args
	     | I_xor args => emit_xor args
	     | I_not(r1, rd) => emit_xnor (r1, REGrand(REG 0), rd)
	     | I_tvs => (emitWord 0w36816; emitWord 0w8199)  (* "tvs 0x7" *)
	     | I_fadd args => emit_fadd args
	     | I_fsub args => emit_fsub args
	     | I_fmul args => emit_fmul args
	     | I_fdiv args => emit_fdiv args
	     | I_fneg args => emit_fneg args
	     | I_fabs args => emit_fabs args
	     | I_fcmp args => emit_fcmp args
	     | I_fmov args => emit_fmov args
	     | I_fitod args => emit_fitod args
(*
	     | _ => ErrorMsg.impossible "[SparcMCEmit.emitInstr]"
*)

    end (* emitInstr *)

    fun comment _ = ()

    fun init n = (code := Word8Array.array(n, 0w0); loc := 0)

end (* structure SparcMCEmit *)

(*
 * $Log: sparcmc.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:47  george
 * Version 110.5
 *
 *)
