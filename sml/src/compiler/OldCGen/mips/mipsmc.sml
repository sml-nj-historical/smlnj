(* mipsmc.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

structure KeepMipsMCode : sig
			      val code : Word8Array.array ref
			      val getCodeString : unit -> Word8Vector.vector
			      val cleanup : unit -> unit
			  end =
struct
    open Word8Array
    val code = ref (array(0,0w0))
    fun getCodeString () = let 
      val s = extract (!code, 0, SOME(length (!code)))
    in 
      code := array(0, 0w0); s
    end
    fun cleanup () = code := array(0,0w0)
end
	
(** NOTE: this isn't the right way to parameterize this structure, since it
 ** doesn't force any connection between MachSpec and E.
 **)
functor MipsMCode(structure MSpec : MACH_SPEC and E : ENDIAN) : EMITTER = struct

  structure M = MipsInstrSet 
  structure K = KeepMipsMCode
  open M

  val error = ErrorMsg.impossible

  val << = Word.<<
  val >> = Word.>>
  val ~>> = Word.~>>
  val || = Word.orb
  val &  = Word.andb
  infix << >> ~>>  || &

  val itow = Word.fromInt

  val loc = ref 0

  fun init n = (K.code := Word8Array.array(n, 0w0); loc := 0)

  fun emitByte' b = let
	val i = !loc
	in
	  loc := i+1;  Word8Array.update (!K.code, i, b)
	end

  fun emitByte n = emitByte'(Word8.fromLargeWord(Word.toLargeWord n))

  fun emitHiLo(hi,lo) = let
    val (byte0,byte1,byte2,byte3) = E.wordLayout (hi,lo)
  in  
    emitByte byte0;
    emitByte byte1;
    emitByte byte2;
    emitByte byte3
  end

  fun emitLongX n = let 
    val w = itow n
  in 
    emitHiLo((w ~>> 0w16) & 0w65535, w & 0w65535)
  end

  fun emitLong n = let 
    val w = itow n
  in 
    emitHiLo((w >> 0w16) & 0w65535, w & 0w65535)
  end

  fun emitString s = Word8Vector.app emitByte' (Byte.stringToBytes s)

  exception BadReal = IEEEReal.BadReal
  val emitReal = emitString o E.order_real o IEEEReal.realconst

  fun emitAddr (INFO{addrOf,...}) (lab,k) = emitLongX (k + addrOf lab - !loc)

  fun define _ _ = ()

  fun mark() = emitLong(LargeWord.toInt(MSpec.ObjDesc.makeDesc(
	(!loc + 4) div 4, MSpec.ObjDesc.tag_backptr)))

  fun comment _ = ()

  fun emitInstr info = let
     val labelValue = M.labelValue info
     val hiLabelValue = M.hiLabelValue info
     val loLabelValue = M.loLabelValue info
     val labBranchOff = M.labBranchOff info

    (* order of operands is identical to instr. format layout *)

      fun R_Type(opcode,rs',rt',rd',shamt,func) = 
	  case (reg_rep rs', reg_rep rt', reg_rep rd')
	   of (Reg' rs, Reg' rt, Reg' rd) =>
	       emitHiLo((itow opcode << 0w10) || (itow rs << 0w5) || itow rt,
			(itow rd << 0w11) || (itow shamt << 0w6) || itow func)
	    | _ => error "MipsMCode.R_Type:"

      fun I_Type(opcode,rs',rt',immed) = 
	  case (reg_rep rs', reg_rep rt')
	   of (Reg' rs, Reg' rt) =>
	        emitHiLo((itow opcode << 0w10) || (itow rs << 0w5) || itow rt,
			 immed)
	    | _ => error "MipsMCode.I_Type:"

      fun R_Type_f(opcode,format,ft',fs',fd',func) = 
	case (reg_rep ft', reg_rep fs', reg_rep fd')
	 of (Freg' ft, Freg' fs, Freg' fd) =>
	     emitHiLo((itow opcode << 0w10) || (itow format << 0w5) || itow ft,
		      (itow fs << 0w11) || (itow fd << 0w6) || itow func)
          | _ => error "MipsMCode.R_Type_f"

      fun I_Type_f(opcode,base',ft',immed) = 
        case (reg_rep base', reg_rep ft') 
	 of (Reg' base, Freg' ft) =>
	     emitHiLo((itow opcode << 0w10) || (itow base << 0w5) || itow ft,
		      immed)
	  | _ => error "MipsMCode.I_Type_f:"	  
	  
      fun immediate_arith (Immed16Op n)    = M.chk_immed16 n
	| immediate_arith (LabelOp labexp) = M.chk_immed16(labelValue labexp)
	| immediate_arith (HiLabOp labexp) = hiLabelValue labexp
	| immediate_arith (LoLabOp labexp) = loLabelValue labexp
	| immediate_arith _ = error "MipsMCode.immediate_arith"

      fun immediate_mem (Immed16Off n) = M.chk_immed16 n
	| immediate_mem (LabOff labexp) = M.chk_immed16(labelValue labexp)
	| immediate_mem (HiLabOff labexp) = hiLabelValue labexp
	| immediate_mem (LoLabOff labexp) = loLabelValue labexp

      fun immediate_branch (opnd as LabOff labexp) = let
	    val labOff = labBranchOff opnd 
          in
	      itow(labOff - ((!loc + 4) div 4))
          end
	| immediate_branch _ = error "MipsMCode.immdiate_branch: bad label"

      fun fcond M.UN   = 0x31
	| fcond M.EQ   = 0x32
	| fcond M.UEQ  = 0x33
	| fcond M.OLT  = 0x34
	| fcond M.ULT  = 0x35
	| fcond M.OLE  = 0x36
	| fcond M.ULE  = 0x37
	| fcond M.NGLE = 0x39
	| fcond M.NGL  = 0x3b
	| fcond M.LT   = 0x3c
	| fcond M.NGE  = 0x3d
	| fcond M.LE   = 0x3e
	| fcond M.NGT  = 0x3f
  in
      fn NOP 		    => emitHiLo(0w0,0w0)

       | SLT(rd,rs,RegOp rt)  => R_Type(0,rs,rt,rd,0,42)
       | SLT(rt,rs,opnd)      => I_Type(10,rs,rt,immediate_arith opnd)
       | SLTU(rd,rs,RegOp rt) => R_Type(0,rs,rt,rd,0,43)
       | SLTU(rt,rs,opnd)     => I_Type(11,rs,rt,immediate_arith opnd)
       | FCMP(cond, fs, ft)   => R_Type_f(17,17,ft,fs,Freg 0, fcond cond)

       | JUMP rs	       => R_Type(0,rs,Reg 0,Reg 0,0,0x8)
       | BEQ(true,rs,rt,opnd)  => I_Type(0x4,rs,rt,immediate_branch opnd)
       | BEQ(false,rs,rt,opnd) => I_Type(0x5,rs,rt,immediate_branch opnd)
       | BCOP1(true, opnd)     => I_Type_f(17,Reg 8,Freg 1,immediate_branch opnd)
       | BCOP1(false, opnd)    => I_Type_f(17,Reg 8,Freg 0,immediate_branch opnd)

       | ADD(rd,rs,RegOp rt)  => R_Type(0,rs,rt,rd,0,0x20)
       | ADD(rt,rs,opnd)      => I_Type(8,rs,rt,immediate_arith opnd)
       | ADDU(rd,rs,RegOp rt) => R_Type(0,rs,rt,rd,0,0x21)
       | ADDU(rt,rs,opnd)     => I_Type(9,rs,rt,immediate_arith opnd)
       | AND(rd,rs,RegOp rt)  => R_Type(0,rs,rt,rd,0,0x24)
       | AND(rt,rs,opnd)      => I_Type(12,rs,rt,immediate_arith opnd)
       | OR(rd,rs,RegOp rt)   => R_Type(0,rs,rt,rd,0,0x25)
       | OR(rt,rs,opnd)       => I_Type(13,rs,rt,immediate_arith opnd)
       | XOR(rd,rs,RegOp rt)  => R_Type(0,rs,rt,rd,0,0x26)
       | XOR(rt,rs,opnd)      => I_Type(14,rs,rt,immediate_arith opnd)
       | SUB(rd,rs,rt)        => R_Type(0,rs,rt,rd,0,0x22)
       | SUBU(rd,rs,rt)	      => R_Type(0,rs,rt,rd,0,0x23)

       | MULT(rs,rt)     => R_Type(0,rs,rt,Reg 0,0,0x18)
       | MULTU(rs,rt)	 => R_Type(0,rs,rt,Reg 0,0,0x19)
       | DIV(rs,rt)      => R_Type(0,rs,rt,Reg 0,0,0x1a)
       | DIVU(rs,rt)     => R_Type(0,rs,rt,Reg 0,0,0x1b)
       | MFHI rd         => R_Type(0,Reg 0,Reg 0,rd,0,0x10)
       | MFLO rd         => R_Type(0,Reg 0,Reg 0,rd,0,0x12)
       | BREAK n         => R_Type(0,Reg 0,Reg n,Reg 0,0,13)

       | ADD_DOUBLE(fd,fs,ft) => R_Type_f(17,17,ft,fs,fd,0)
       | SUB_DOUBLE(fd,fs,ft) => R_Type_f(17,17,ft,fs,fd,1)
       | MUL_DOUBLE(fd,fs,ft) => R_Type_f(17,17,ft,fs,fd,2)
       | DIV_DOUBLE(fd,fs,ft) => R_Type_f(17,17,ft,fs,fd,3)
       | MOV_DOUBLE(fd,fs)    => R_Type_f(17,17,Freg 0,fs,fd,6)
       | NEG_DOUBLE(fd,fs)    => R_Type_f(17,17,Freg 0,fs,fd,7)
       | ABS_DOUBLE(fd,fs)    => R_Type_f(17,17,Freg 0,fs,fd,5)
       | CVTI2D(fd,fs)        => R_Type_f(17,20,Freg 0,fs,fd,0x21)
       | MTC1(rt,fs)      => 
	          (case reg_rep rt
		    of Reg' rt' => R_Type_f(17,4, Freg rt',fs,Freg 0,0)
		     | _ => error "MipsMCode.emitInstr: MTC1")

       | LBU(rt,base,opnd)  => I_Type(0x24,base,rt,immediate_mem opnd)
       | SB(rt,base,opnd)   => I_Type(0x28,base,rt,immediate_mem opnd)
       | LW(rt,base,opnd)   => I_Type(0x23,base,rt,immediate_mem opnd)
       | SW(rt,base,opnd)   => I_Type(0x2b,base,rt,immediate_mem opnd)
       | LWC1(ft,base,opnd) => I_Type_f(0x31,base,ft,immediate_mem opnd)
       | SWC1(ft,base,opnd) => I_Type_f(0x39,base,ft,immediate_mem opnd)	
       | LUI(rt,opnd)       => I_Type(0xf,Reg 0,rt,immediate_mem opnd)

       | SLL(rd,rt,Int5 n) => R_Type(0,Reg 0,rt,rd,n,0)
       | SLLV(rd,rt,rs)    => R_Type(0,rs,rt,rd,0,4)
       | SRA(rd,rt,Int5 n) => R_Type(0,Reg 0,rt,rd,n,3)
       | SRAV(rd,rt,rs)    => R_Type(0,rs,rt,rd,0,7)
       | SRL(rd,rt,Int5 n) => R_Type(0,Reg 0,rt,rd,n,2)
       | SRLV(rd,rt,rs)    => R_Type(0,rs,rt,rd,0,6)
  end (* local *)

end



(*
 * $Log: mipsmc.sml,v $
 * Revision 1.3  1998/02/12 22:18:49  jhr
 *   Finished removing references to System.Tags in MIPS code generators.
 *
 * Revision 1.2  1997/08/25 16:42:14  jhr
 *   Improved implementation of emitString.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:39  george
 *   Version 109.24
 *
 *)
