(* ppcMC.sml
 *
 * COPYRIGHT (c) 1999 Lucent Technologies, Bell Labs.
 *
 *)

(** IBM PPC machine code generator **)

functor PPCMCEmitter
  (structure Instr : PPCINSTR
   structure PseudoOps : PSEUDO_OPS
   structure CodeString : CODE_STRING) : EMITTER_NEW =
struct
  structure I = Instr
  structure P = PseudoOps 
  structure C = I.C

  structure LE = LabelExp

  val << = Word.<<
  val >> = Word.>>
  val ~>> = Word.~>>
  val ++ = Word.orb
  val & = Word.andb
  infix << >> ~>> ++ &  

  val itow  = Word.fromInt
  fun error msg = MLRiscErrorMsg.impossible ("PPCMCEmitter." ^ msg)
  val loc = ref 0


  fun eBytefromW8 w8  = let
    val i = !loc
  in loc := i+1; CodeString.update(i,w8) 
  end

  fun eBytefromW w = let
    val i = !loc
    val wtob  = Word8.fromLargeWord o Word.toLargeWord
  in loc:= i+1; CodeString.update(i, wtob w)
  end

  fun emitHiLo(hi,lo) = 
    (eBytefromW ((hi >> 0w8) & 0w255);
     eBytefromW (hi & 0w255);
     eBytefromW ((lo >> 0w8) & 0w255);
     eBytefromW (lo & 0w255))

  fun defineLabel lab = ()
  fun pseudoOp pOp = P.emitValue{pOp=pOp, loc= !loc, emit=eBytefromW8}
  fun comment msg = ()
  fun init n = (CodeString.init n; loc:=0)

  fun emitInstr(instr, regmap) = let
    val rMap = Intmap.map regmap
    fun rNum r = itow(rMap r)
    val fNum = rNum

    val don'tCare = 0w0

    fun bitToInt(I.LT) = 0      | bitToInt(I.GT) = 1
      | bitToInt(I.EQ) = 2      | bitToInt(I.SO) = 3
      | bitToInt(I.FL) = 0      | bitToInt(I.FG) = 1
      | bitToInt(I.FE) = 2      | bitToInt(I.FU) = 3
      | bitToInt(I.FX) = 0      | bitToInt(I.FEX)= 1
      | bitToInt(I.VX) = 2      | bitToInt(I.OX) = 3

    fun cvtRc true = 0w1
      | cvtRc false = 0w0

    fun cvtOE true = 0w1
      | cvtOE false = 0w0

    fun cvtLK true = 0w1
      | cvtLK false = 0w0

    fun cvtBO I.TRUE = 0w12		(* 011zy *)
      | cvtBO I.FALSE = 0w4		(* 001zy *)
      | cvtBO I.ALWAYS = 0w20		(* 1z1zz *)
      | cvtBO(I.COUNTER{eqZero, cond=NONE}) =
         if eqZero then 0w18		(* 1z01y *)
	 else 0w16			(* 1z00y *)
      | cvtBO(I.COUNTER{eqZero, cond=SOME cc}) = 
	(case (eqZero, cc)
	  of (false, false) => 0w0	(* 0000y *)
	   | (false, true) => 0w8	(* 0100y *)
    	   | (true, false) => 0w2	(* 0001y *)
	   | (true, true) => 0w10	(* 0101y *)
	 (*esac*))

    fun cr_bit(cr, bit) = itow(cr*4 + bitToInt bit)

    local
      fun split i = let
	    val w = Word.fromInt i
	    val hi = Word.~>>(w, 0w16)
	    val lo = Word.andb(w, 0w65535)
      in if lo <  0w32768 then (hi, lo) else (hi+0w1, lo-0w65536)
      end
    in
      fun high n = #1 (split n)
      fun low n  = #2 (split n)
    end


    fun operand(I.RegOp r) = error "operand:RegOp"
      | operand(I.ConstOp c) = error "operand:ConstOp"
      | operand(I.ImmedOp i) = itow i
      | operand(I.LabelOp lexp) = itow(LE.valueOf lexp)

    fun relative (I.LabelOp lexp) = itow((LE.valueOf lexp - (!loc)) div 4)
      | relative _ = error "relative"

    fun d_form(opcd,rt,ra,si) = let 
      val hi = (opcd << 0w10) ++ (rt << 0w5) ++ ra
      val lo = si
    in emitHiLo(hi,lo)
    end

    fun b_form(opcd,bo,bi,bd,aa,lk) = let
      val hi = (opcd << 0w10) ++ (bo << 0w5) ++ bi
      val lo = (bd << 0w2) ++ (aa << 0w1) ++ lk
    in emitHiLo(hi,lo)
    end

    fun x_form(opcd,rt,ra,rb,eo,rc) = let
      val hi = (opcd << 0w10) ++ (rt << 0w5) ++ ra
      val lo = (rb << 0w11) ++ (eo << 0w1) ++ rc
    in emitHiLo(hi,lo)
    end

    fun xl_form(opcd,bt,ba,bb,eo,lk) = let
      val hi = (opcd << 0w10) ++ (bt << 0w5) ++ ba
      val lo = (bb << 0w11) ++ (eo << 0w1) ++ lk
    in emitHiLo(hi,lo)
    end

    fun xo_form(opcd,rt,ra,rb,oe,eo',rc) = let
      val hi = (opcd << 0w10) ++ (rt << 0w5) ++ ra
      val lo = (rb << 0w11) ++ (oe << 0w10) ++ (eo' << 0w1) ++ rc
    in emitHiLo(hi,lo)
    end

    fun a_form(opcd,frt,fra,frb,frc,xo,rc) = let
      val hi = (opcd << 0w10) ++ (frt << 0w5) ++ fra
      val lo = (frb <<0w11) ++ (frc <<0w6) ++ (xo <<0w1) ++ rc
    in emitHiLo(hi,lo)
    end

    fun m_form(opcd,rs,ra,rb,mb,me,rc) = let
      val hi = (opcd << 0w10) ++ (rs << 0w5) ++ ra
      val lo = (rb << 0w11) ++ (itow mb << 0w6) ++ (itow me << 0w1) ++ rc
    in emitHiLo(hi,lo)
    end

    fun ds_form(opcd, rt, ra, ds, xo) = let
      val hi = (opcd << 0w10) ++ (rt << 0w5) ++ ra
      val lo = (ds << 0w2) ++ xo
    in emitHiLo(hi, lo)
    end

    fun xfx_form(opcd, rs, spr, xo, LK) = let
      val hi = (opcd << 0w10) ++ (rs << 0w5) ++ (spr & 0wx1f)
      val lo = ((spr >> 0w5) << 0w11) ++ (xo << 0w1) ++ LK
    in emitHiLo(hi, lo)
    end

    fun i_form(opcd,li,lk) = let
      val liLo = li & 0wx3fff
      val liHi = (li ~>> 0w14) & 0wx3ff
      val hi = (opcd << 0w10) ++ liHi
      val lo = (liLo << 0w2) ++ lk
    in emitHiLo(hi,lo)
    end

  in
    case instr
    of I.L{sz, rt, ra, d=I.RegOp rb, mem} => let
	 val ra = rNum ra
	 val rb = rNum rb
       in
	 case sz 
	 of I.Byte => x_form(0w31, rNum rt, ra, rb, 0w87, don'tCare)
	  | I.Half => x_form(0w31, rNum rt, ra, rb, 0w279, don'tCare)
	  | I.Word => x_form(0w31, rNum rt, ra, rb, 0w23, don'tCare)
	  | I.Long => x_form(0w31, rNum rt, ra, rb, 0w21, don'tCare)
	  | I.Single => x_form(0w31, fNum rt, ra, rb, 0w535, don'tCare)
	  | I.Double => x_form(0w31, fNum rt, ra, rb, 0w599, don'tCare)
	 (*esac*)
       end
     | I.L{sz, rt, ra, d, mem} => let
         val ra = rNum ra
	 val d = operand d
       in
	 case sz 
	 of I.Byte => d_form(0w34, rNum rt, ra, d)
	  | I.Half => d_form(0w40, rNum rt, ra, d)
	  | I.Word => d_form(0w32, rNum rt, ra, d)
	  | I.Long => ds_form(0w58, rNum rt, ra, d, 0w0)
	  | I.Single => d_form(0w48, fNum rt, ra, d)
	  | I.Double => d_form(0w50, fNum rt, ra, d)
	 (*esac*)
       end
     | I.ST{sz, rs, ra, d=I.RegOp rb, mem} => let
	 val ra=rNum ra
	 val rb=rNum rb
       in
	 case sz 
	 of I.Byte => x_form(0w31, rNum rs, ra, rb, 0w215, don'tCare)
	  | I.Half => x_form(0w31, rNum rs, ra, rb, 0w407, don'tCare)
	  | I.Word => x_form(0w31, rNum rs, ra, rb, 0w151, don'tCare)
	  | I.Long => x_form(0w31, rNum rs, ra, rb, 0w149, don'tCare)
	  | I.Single => x_form(0w31, fNum rs, ra, rb, 0w663, don'tCare) 
	  | I.Double => x_form(0w31, fNum rs, ra, rb, 0w727, don'tCare)
	 (*esac*)
       end
     | I.ST{sz, rs, ra, d, mem} => let
         val ra = rNum ra
	 val d = operand d
       in
	 case sz 
	 of I.Byte => d_form(0w38, rNum rs, ra, d)
	  | I.Half => d_form(0w44, rNum rs, ra, d)
	  | I.Word => d_form(0w36, rNum rs, ra, d)
	  | I.Long => ds_form(0w62, rNum rs, ra, d, 0w0)
	  | I.Single => d_form(0w52, fNum rs, ra, d)
	  | I.Double => d_form(0w54, fNum rs, ra, d)
	 (*esac*)
       end
     | I.UNARY{oper,rt,ra,Rc,OE} => 
	(case oper
	  of I.NEG => 
	      xo_form(0w31, rNum rt, rNum ra, don'tCare, cvtOE OE, 0w104, cvtRc Rc)
        (*esac*))
     | I.ARITH{oper,rt,ra,rb,Rc,OE} => let
	 val rt=rNum rt
	 val ra=rNum ra
	 val rb=rNum rb
	 val Rc=cvtRc Rc
	 val OE=cvtOE OE
       in
	 case oper
	  of I.ADD  => xo_form(0w31, rt, ra, rb, OE, 0w266, Rc)
	   | I.ADDS => error "emitInstr:ARITH:ADDS"
           | I.SUBF => xo_form(0w31, rt, ra, rb, OE, 0w40, Rc)
	   | I.MULL => xo_form(0w31, rt, ra, rb, OE, 0w235, Rc)
	   | I.DIVW => xo_form(0w31, rt, ra, rb, OE, 0w491, Rc)
	   | I.DIVWU=> xo_form(0w31, rt, ra, rb, OE, 0w459, Rc)
	   | I.AND  => x_form(0w31, ra, rt, rb, 0w28, Rc)
	   | I.OR   => x_form(0w31, ra, rt, rb, 0w444, Rc)
	   | I.XOR  => x_form(0w31, ra, rt, rb, 0w316, Rc)
	   | I.XORS => error "emitInstr:ARITH:XORS"
	   | I.SLW  => x_form(0w31, ra, rt, rb, 0w24, Rc)
	   | I.SRW  => x_form(0w31, ra, rt, rb, 0w536, Rc)
	   | I.SRAW => x_form(0w31, ra, rt, rb, 0w792, Rc)
	(*esac*)
       end
     | I.ARITHI{oper, rt=rt', ra=ra', im=im'} => let
         val rt=rNum rt'
	 val ra=rNum ra'
	 val im=operand im'
       in
	 case oper
	  of I.ADD  => d_form(0w14, rt, ra, im)
	   | I.ADDS => d_form(0w15, rt, ra, im)
           | I.SUBF => d_form(0w8, rt, ra, im)
	   | I.MULL => d_form(0w7, rt, ra, im)
	   | I.DIVW => error "emitInstr:ARITHI:DIVW"
	   | I.DIVWU => error "emitInstr:ARITHI:DIVWU"
	   | I.AND => d_form(0w28, ra, rt, im)
	   | I.OR => d_form(0w24, ra, rt, im) 
	   | I.XOR => d_form(0w26, ra, rt, im)
	   | I.XORS => d_form(0w27, ra, rt, im)
	   | I.SLW => 
             (case im'
	      of I.ImmedOp n =>
		  emitInstr(
		    I.ROTATE{oper=I.RLWNM, ra=rt', rs=ra', sh=I.ImmedOp n,
			     mb=0, me=31-n},
		    regmap)
	       | _ => error "emitInstr:ARITHI:SLW"
	     (*esac*))
     	   | I.SRW => 
	     (case im'
	      of I.ImmedOp n =>
		  emitInstr(
	            I.ROTATE{oper=I.RLWNM, ra=rt', rs=ra', sh=I.ImmedOp(32-n), 
			     mb=n, me=31},
                    regmap)
	       | _ => error "emitInstr:ARITHI:SLW"
	      (*esac*))
	   | I.SRAW => x_form(0w31, ra, rt, im, 0w824, 0w0)
	(*esac*)
       end
     | I.ROTATE{oper, ra, rs, sh=I.RegOp rb, mb, me} => 
         m_form(0w23, rNum rs, rNum ra, rNum rb, mb, me, don'tCare)
     | I.ROTATE{oper, ra, rs, sh, mb, me} => 
	 m_form(0w21, rNum rs, rNum ra, operand sh, mb, me, don'tCare)
     | I.COMPARE{cmp, bf, ra, rb=I.RegOp rb} => let
	 val ra=rNum ra
	 val rb=rNum rb
	 val bf=itow(bf*4)
       in
	 case cmp
	 of I.CMP => x_form(0w31, bf, ra, rb, 0w0, don'tCare)
          | I.CMPL => x_form(0w31,bf, ra, rb, 0w32, don'tCare)
         (*esac*)
       end
     | I.COMPARE{cmp, bf, ra, rb} => let
        val ra=rNum ra
	val rb=operand rb
	val bf=itow(bf * 4)
       in
	 case cmp
	 of I.CMP => d_form(0w11, bf, ra, rb)
          | I.CMPL => d_form(0w10, bf, ra, rb) 
         (*esac*)
       end
     | I.FCOMPARE{cmp, bf, fa, fb} => let
	val fa=fNum fa
	val fb=fNum fb
	val bf=itow(bf*4)
       in
	case cmp
	 of I.FCMPO => x_form(0w63, bf, fa, fb, 0w32, don'tCare)
          | I.FCMPU => x_form(0w63, bf, fa, fb, 0w0, don'tCare)
	(*esac*)
       end
     | I.FUNARY{oper, ft, fb, Rc} => let
         val ft=fNum ft
	 val fb=fNum fb
	 val Rc=cvtRc Rc
       in
	 case oper
         of I.FMR => x_form(0w63, ft, don'tCare, fb, 0w72, Rc)
          | I.FABS => x_form(0w63, ft, don'tCare, fb, 0w264, Rc)
          | I.FNEG => x_form(0w63, ft, don'tCare, fb, 0w40, Rc)
	(*esac*)
       end
     | I.FARITH{oper, ft, fa, fb, Rc} => let
         val fa=fNum fa
	 val fb=fNum fb
	 val ft=fNum ft
	 val Rc=cvtRc Rc
       in
	 case oper
	 of I.FADD => a_form(0w63, ft, fa, fb, don'tCare, 0w21, Rc)
          | I.FSUB => a_form(0w63, ft, fa, fb, don'tCare, 0w20, Rc)
          | I.FMUL => a_form(0w63, ft, fa, don'tCare, fb, 0w25, Rc)
          | I.FDIV => a_form(0w63, ft, fa, fb, don'tCare, 0w18, Rc)
	 (*esac*)
       end
     | I.CCARITH{oper, bt, ba, bb} => let
        val bt=cr_bit bt
	val ba=cr_bit ba
	val bb=cr_bit bb
       in
	 case oper
	 of I.CRAND => xl_form(0w19, bt, ba, bb, 0w257, don'tCare)
	  | I.CROR => xl_form(0w19, bt, ba, bb, 0w449, don'tCare)
	  | I.CRXOR => xl_form(0w19, bt, ba, bb, 0w193, don'tCare)
	  | I.CRNAND => xl_form(0w19, bt, ba, bb, 0w225, don'tCare)
	  | I.CRNOR => xl_form(0w19, bt, ba, bb, 0w33, don'tCare)
	 (*esac*)
       end
     | I.MCRF {bf:int, bfa:int} => 
         xl_form(0w19, itow(bf*4), itow(bfa*4), don'tCare, 0w0, don'tCare)
     | I.MTSPR{rs:int, spr:int} => 
         xfx_form(0w31, rNum rs, itow spr, 0w467, don'tCare)
     | I.MFSPR{rt:int, spr:int} => 
         xfx_form(0w31, rNum rt, itow spr, 0w339, don'tCare)
     | I.TWI{to, ra, si} => 
	 d_form(0w3, itow to, rNum ra, operand si)

    (* Control Instructions -  AA is always assumed to be 0 *)
     | I.BC{bo, bf, bit, addr, LK, ...} => 
	 b_form(0w16, cvtBO bo, cr_bit(bf, bit), relative addr, 0w0, cvtLK LK)
     | I.BCLR{bo, bf, bit, LK, labels} =>
	 xl_form(0w19, cvtBO bo, cr_bit(bf, bit), don'tCare, 0w16, cvtLK LK)
     | I.B{addr, LK} => i_form(0w18, relative addr, cvtLK LK)

    (* CALL = BCLR {bo=ALWAYS, bf=0, bit=0, LK=true, labels=[] *)
     | I.CALL{def, use} =>
         emitInstr(
	   I.BCLR{bo=I.ALWAYS, bf=0, bit=I.LT, LK=true, labels=[]},
	   regmap)

     | I.COPY{dst, src, impl, tmp} => error "emitInstr:COPY"
     | I.FCOPY{dst, src, impl, tmp}=> error "emitInstr:FCOPY"
  end(*emitInstr*)
end (*functor*)
