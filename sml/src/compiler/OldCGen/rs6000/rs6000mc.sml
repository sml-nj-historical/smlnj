(* rs6000mc.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

(** IBM RS6000 machine code generator **)
structure KeepRS6000MCode : sig
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
	
structure RS6000MCodeEmitter : EMITTER = 
struct

  structure M = RS6000InstrSet 
  structure K = KeepRS6000MCode
  open M

  fun error msg = ErrorMsg.impossible ("RS6000MCodeEmitter." ^ msg)

  val << = Word.<<
  val >> = Word.>>
  val ~>> = Word.~>>
  val || = Word.orb
  val &  = Word.andb
  infix << >> ~>> || &

  val itow = Word.fromInt

  val loc = ref 0

  fun init n = (K.code := Word8Array.array(n, 0w0); loc := 0)

  fun emitByte' b = let
	val i = !loc
	in
	  loc := i+1;  Word8Array.update (!K.code, i, b)
	end

  fun emitByte n = emitByte'(Word8.fromLargeWord(Word.toLargeWord n))

  fun emitHiLo(hi,lo) = ( emitByte ((hi >> 0w8) & 0w255);
			  emitByte (hi & 0w255);
			  emitByte ((lo >> 0w8) & 0w255);
			  emitByte (lo & 0w255))


  fun emitLong n = let
    val w = itow n
  in
    emitHiLo((w >> 0w16), w & 0w65535)
  end

  fun emitLongX n = let
    val w = itow n
  in
    emitHiLo((w ~>> 0w16), w & 0w65535)
  end

  fun emitString s = Word8Vector.app emitByte' (Byte.stringToBytes s)

  exception BadReal = IEEEReal.BadReal
  val emitReal = emitString o IEEEReal.realconst

  fun emitAddr (INFO{addrOf,...}) (lab,k) = emitLongX (k + addrOf lab - !loc)

  fun define _ _ = ()

  fun mark() = emitLong(LargeWord.toInt(RS6000Spec.ObjDesc.makeDesc(
	(!loc + 4) div 4,
	RS6000Spec.ObjDesc.tag_backptr)))

  fun comment _ = ()

  fun emitInstr info = let
      val don'tCare = 0
      fun d_form(opcd,rt,ra,si) = let 
	  val hi = (opcd << 0w10) || (itow rt << 0w5) || itow ra
	  val lo = si
	in 
	    emitHiLo(hi,lo)
	end

      fun b_form(opcd,bo,bi,bd,aa,lk) = let
	  val hi = (opcd << 0w10) || (itow bo << 0w5) || itow bi
	  val lo = (itow bd << 0w2) || (itow aa << 0w1) || itow lk
	in
	    emitHiLo(hi,lo)
	end

      fun i_form(opcd,li,aa,lk) = let
	  val liLo = itow li & 0wx3fff
	  val liHi = (itow li ~>> 0w14) & 0wx3ff
	  val hi = (opcd << 0w10) || liHi
	  val lo = (liLo << 0w2) || (itow aa << 0w1) || itow lk
	in
	    emitHiLo(hi,lo)
	end

      fun x_form(opcd,rt,ra,rb,eo,rc) = let
	  val hi = (opcd << 0w10) || (itow rt << 0w5) || itow ra
	  val lo = (itow rb << 0w11) || (itow eo << 0w1) || itow rc
	in
	    emitHiLo(hi,lo)
	end

      fun xl_form(opcd,bt,ba,bb,eo,lk) = let
	  val hi = (opcd << 0w10) || (itow bt << 0w5) || itow ba
	  val lo = (itow bb << 0w11) || (itow eo << 0w1) || itow lk
	in
	    emitHiLo(hi,lo)
	end

      fun xo_form(opcd,rt,ra,rb,oe,eo',rc) = let
	  val hi = (opcd << 0w10) || (itow rt << 0w5) || itow ra
	  val lo = (itow rb << 0w11) || (itow oe << 0w10) || 
			(itow eo' << 0w1) || itow rc
	in
	    emitHiLo(hi,lo)
	end

      fun a_form(opcd,frt,fra,frb,frc,xo,rc) = let
	  val hi = (opcd << 0w10) || (itow frt << 0w5) || itow fra
	  val lo = (itow frb << 0w11) || (itow frc << 0w6)
			|| (itow xo << 0w1) || itow rc
	in
	    emitHiLo(hi,lo)
	end

      fun m_form(opcd,rs,ra,rb,mb,me,rc) = let
	  val hi = (opcd << 0w10) || (itow rs << 0w5) || itow ra
	  val lo = (itow rb << 0w11) || (itow mb << 0w6) || 
			(itow me << 0w1) || itow rc
	in
	    emitHiLo(hi,lo)
	end


      fun cr_bits M.LT = 0
	| cr_bits M.GT = 1
	| cr_bits M.EQ = 2
	| cr_bits M.SO = 3
	| cr_bits _    = error "cr_bits"

      fun fcr_bits(M.FL,2)  = 8
	| fcr_bits(M.FG,2)  = 9
	| fcr_bits(M.FE,2)  = 10
	| fcr_bits(M.UN,2)  = 11
	| fcr_bits(M.FX,1)  = 4
	| fcr_bits(M.FEX,1) = 5
	| fcr_bits(M.VX,1)  = 6
	| fcr_bits(M.OX,1)  = 7
	| fcr_bits _ = error "fcr_bits"

      fun immedLabOff labexp = let  val labOff = M.labelValue info labexp 
			       in
				   (labOff - !loc) div 4
			       end

      fun emitBranchcc (bool,Label16Off labexp,crbit) = let
	  val bo = if bool then 0x0c else 0x4
	  val lab = immedLabOff labexp
	in
	    b_form(0w16,bo,crbit,lab,0,0)
	end

      fun immed_eaValue(Immed16Op n) = itow n
	| immed_eaValue(LabelOp lab) = itow(M.labelValue info lab)
	| immed_eaValue(HiLabOp lab) = M.hiLabelValue info lab
	| immed_eaValue(LoLabOp lab) = M.loLabelValue info lab
	| immed_eaValue _ = error "immed_eaValue"
  in
      fn NOP 				 => error "emitInstr: NOP"
       | B lab24exp => let
           val lab = M.labBranch24Off info lab24exp - (!loc div 4)
	 in 
	     i_form(0w18,lab,0,0)
	 end
       | BB(cc,bool,lab)		=> emitBranchcc(bool,lab,cr_bits cc)
       | BBF(cc,cr,bool,lab) 		=> emitBranchcc(bool,lab,fcr_bits(cc,cr))
       | BR() 		   	     => xl_form(0w19,0x14,don'tCare,don'tCare,16,0)
       | LBZ(Reg rt,Reg ra,RegOp(Reg rb))=> x_form(0w31,rt,ra,rb,87,0)
       | LBZ(Reg rt,Reg ra,ea)   	=> d_form(0w34,rt,ra,immed_eaValue ea)
       | L(Reg rt,Reg ra,RegOp(Reg rb)) => x_form(0w31,rt,ra,rb,23,0)
       | L(Reg rt,Reg ra,ea) 	   	=> d_form(0w32,rt,ra,immed_eaValue ea)
       | LFD(Freg frt,Reg ra,ea) 	=> d_form(0w50,frt,ra,immed_eaValue ea)
       | LIU(_,RegOp _)			=> error "emitInstr: LIU"
       | LIU(Reg rt,ui) 	   	=> d_form(0w15,rt,0,immed_eaValue ui)
       | MTSPR(LR,Reg rs) 	        => x_form(0w31,rs,0x8,don'tCare,467,0)
       | MTSPR(MQ,Reg rs) 	        => x_form(0w31,rs,0,don'tCare,467,0)
       | FMR(Freg frt,Freg frb)         => x_form(0w63,frt,don'tCare,frb,72,0)
       | MTFSB1 bt 	                => x_form(0w63,bt,don'tCare,don'tCare,38,1)
       | TRAP() 			=> x_form(0w31,4,0,0,4,0)
       | CAL(Reg rt,Reg ra,RegOp(Reg rb)) => xo_form(0w31,rt,ra,rb,0,266,0)
       | CAL(Reg rt,Reg ra,ea)		=> d_form(0w14,rt,ra,immed_eaValue ea)
       | STB(Reg rs,Reg ra,RegOp(Reg rb))=> x_form(0w31,rs,ra,rb,215,0)
       | STB(Reg rs,Reg ra,ea) 	         => d_form(0w38,rs,ra,immed_eaValue ea)
       | ST(Reg rs,Reg ra,RegOp(Reg rb)) => x_form(0w31,rs,ra,rb,151,0)
       | ST(Reg rs,Reg ra,ea)  	         => d_form(0w36,rs,ra,immed_eaValue ea)
       | STFD(Freg frs,Reg ra,ea) 	 => d_form(0w54,frs,ra,immed_eaValue ea)

       | A(Reg rt,Reg ra,RegOp(Reg rb))  => xo_form(0w31,rt,ra,rb,0,10,0)
       | A(Reg rt,Reg ra,ea) 	      	 => d_form(0w12,rt,ra,immed_eaValue ea)
       | AO(Reg rt,Reg ra,Reg rb) 	 => xo_form(0w31,rt,ra,rb,1,10,1)
       | FAO(Freg frt,Freg fra,Freg frb) => a_form(0w63,frt,fra,frb,don'tCare,21,1)

       | SF(Reg rt,Reg ra,RegOp(Reg rb)) => xo_form(0w31,rt,ra,rb,0,8,0)
       | SF(Reg rt,Reg ra,ea)            => d_form(0w8,rt,ra,immed_eaValue ea)
       | SFO(Reg rt,Reg ra,Reg rb) 	 => xo_form(0w31,rt,ra,rb,1,8,1)
       | FSO(Freg frt,Freg fra,Freg frb) => a_form(0w63,frt,fra,frb,don'tCare,20,1)

       | MULSO(Reg rt,Reg ra,Reg rb) 	 => xo_form(0w31,rt,ra,rb,1,235,1)
       | MULS(Reg rt,Reg ra,Reg rb)	 => xo_form(0w31,rt,ra,rb,0,235,0)
       | FMO(Freg frt,Freg fra,Freg frc) => a_form(0w63,frt,fra,don'tCare,frc,25,1)

       | DIVS(Reg rt,Reg ra,Reg rb) 	 => xo_form(0w31,rt,ra,rb,1,363,1)
       | DIV(Reg rt,Reg ra,Reg rb) 	 => xo_form(0w31,rt,ra,rb,0,331,0)
       | FDO(Freg frt,Freg fra,Freg frb) => a_form(0w63,frt,fra,frb,don'tCare,18,1)

       | FNEG(Freg frt,Freg frb) 	 => x_form(0w63,frt,don'tCare,frb,40,1)
       | FABS(Freg frt,Freg frb) 	 => x_form(0w63,frt,don'tCare,frb,264,1)

       | CMP(Reg ra,RegOp(Reg rb))       => x_form(0w31,0,ra,rb,0,0)
       | CMP(Reg ra,ea)          	 => d_form(0w11,0,ra,immed_eaValue ea)
       | CMPL(Reg ra,Reg rb) 		 => x_form(0w31,0,ra,rb,32,0)
       | FCMP(Freg fra,Freg frb) 	 => x_form(0w63,8,fra,frb,32,0)
       | CROR(bt, ba, bb)                => 
	   xl_form(0w19, fcr_bits(bt,2), fcr_bits(ba,2), fcr_bits(bb,2), 449, 0)

       | AND(Reg ra,Reg rs,RegOp(Reg rb)) => x_form(0w31,rs,ra,rb,28,0)
       | AND(Reg ra,Reg rs, ea)  	  => d_form(0w28,rs,ra,immed_eaValue ea)
       | OR(Reg ra,Reg rs,RegOp(Reg rb))  => x_form(0w31,rs,ra,rb,444,0)
       | OR(Reg ra,Reg rs,ea) 	          => d_form(0w24,rs,ra,immed_eaValue ea)
       | XOR(Reg ra,Reg rs,RegOp(Reg rb)) => x_form(0w31,rs,ra,rb,316,0)
       | XOR(Reg ra,Reg rs,ea) 	          => d_form(0w26,rs,ra,immed_eaValue ea)
       | XORU(_,_,RegOp _) 		  => error "emitInstr: XORU"
       | XORU(Reg ra,Reg rs,ea)		  => d_form(0w27,rs,ra,immed_eaValue ea)

       | SL(Reg ra,Reg rs,RegShift(Reg rb))  => x_form(0w31,rs,ra,rb,24,0)
       | SL(Reg ra,Reg rs,Int5Shift si)      => m_form(0w21,rs,ra,si,0,31-si,0)
       | SRA(Reg ra,Reg rs,RegShift(Reg rb)) => x_form(0w31,rs,ra,rb,792,0)
       | SRA(Reg ra,Reg rs,Int5Shift si)     => x_form(0w31,rs,ra,si,824,0)
       | SRL(Reg ra,Reg rs,Int5Shift si)     => x_form(0w31,rs,ra,si,696,0)
       | SRL(Reg ra,Reg rs,RegShift(Reg rb)) => x_form(0w31,rs,ra,rb,665,0)
       | _ => error "emitInstr"
  end 
end


(*
 * $Log: rs6000mc.sml,v $
 * Revision 1.4  1998/02/12 20:48:44  jhr
 *   Removed references to System.Tags.
 *
 * Revision 1.3  1997/11/14 21:48:10  jhr
 *   Restored the support for the Power architecture; the PowerPC code
 *   generator will be MLRisc based.
 *
 * Revision 1.2  1997/08/25  16:43:35  jhr
 *   Replaced some old Power architecture instructions with PowerPC instructions.
 *   This means that the Power architecture is no longer supported by this
 *   code generator.  Also improved implementation of emitString.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:45  george
 *   Version 109.24
 *
 *)
