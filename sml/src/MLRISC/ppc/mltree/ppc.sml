functor PPC
  (structure PPCInstr : PPCINSTR
   structure PPCMLTree : MLTREE 
      where Region = PPCInstr.Region
        and Constant = PPCInstr.Constant
   structure Flowgen : FLOWGRAPH_GEN 
      where I = PPCInstr
        and T = PPCMLTree
        and B = PPCMLTree.BNames
   structure PseudoInstrs : PPC_PSEUDO_INSTR 
      where I = PPCInstr 
   val rs6000flag : bool
  ) : MLTREECOMP = 
struct
  structure I = PPCInstr
  structure F = Flowgen
  structure T = PPCMLTree
  structure C = PPCInstr.C
  structure LE = LabelExp
  structure W32 = Word32

  (* label where trap is generated *)
  val trapLabel : Label.label option ref = ref NONE 
  (* true if a trap label is required  *)
  val trapsPresent = ref false

  fun error msg = MLRiscErrorMsg.impossible ("PPC." ^ msg)

  val newReg = C.newReg
  val newFreg = C.newFreg
  val newCCreg = C.newCCreg
  val emit = F.emitInstr

  val emitInstr = emit

  fun signed16 i = ~32768 <= i andalso i < 32768
  fun unsigned16 i = 0 <= i andalso i < 65536

  fun emitBranch{bo, bf, bit, addr, LK} = let
    val fallThrLab = Label.newLabel""
    val fallThrOpnd = I.LabelOp(LE.LABEL fallThrLab)
  in
    emit(I.BC{bo=bo, bf=bf, bit=bit, addr=addr, LK=LK, fall=fallThrOpnd});
    F.defineLabel fallThrLab
  end

  fun split n = let
    val wtoi = Word.toIntX
    val w = Word.fromInt n
    val hi = Word.~>>(w, 0w16)
    val lo = Word.andb(w, 0w65535)
    val (high, low) = if lo < 0w32768 then (hi, lo) else (hi+0w1, lo-0w65536)
  in (wtoi high, wtoi low)
  end

  fun loadImmedHiLo(0, lo, rt) =
        emit(I.ARITHI{oper=I.ADD, rt=rt, ra=0, im=I.ImmedOp lo})
    | loadImmedHiLo(hi, lo, rt) = 
       (emit(I.ARITHI{oper=I.ADDS, rt=rt, ra=0, im=I.ImmedOp hi});
	if lo = 0 then () 
	else emit(I.ARITHI{oper=I.ADD, rt=rt, ra=rt, im=I.ImmedOp lo}))

  fun loadImmed(n, rt) = 
    if signed16 n then
       emit(I.ARITHI{oper=I.ADD, rt=rt, ra=0 , im=I.ImmedOp n})
    else let 
        val (hi, lo) = split n
      in loadImmedHiLo(hi, lo, rt)
      end

  fun immedOpnd range (e1, e2 as T.LI i, _) =
       (reduceExp e1, 
	if range i then I.ImmedOp i else I.RegOp(reduceExp e2))
    | immedOpnd _ (e1, T.CONST c, _) = (reduceExp e1, I.ConstOp c)
    | immedOpnd _ (e1, T.LABEL lexp, _) = (reduceExp e1, I.LabelOp lexp)
    | immedOpnd range (e1, e2 as T.LI32 w, _) = let
        fun opnd2() = I.RegOp(reduceExpRd(e2, newReg()))
      in
       (reduceExp e1,
	let val i = Word32.toIntX w
        in if range i then I.ImmedOp i else opnd2()
        end handle Overflow => opnd2())
      end
    | immedOpnd _ (e1, e2, T.LR) = (reduceExp e1, I.RegOp(reduceExp e2))
    | immedOpnd _ (e1, e2, T.RL) = let
	val r2 = I.RegOp(reduceExp e2)
      in (reduceExp e1, r2)
      end

  and commImmedOpnd range (e1 as T.LI _, e2, ord) = 
       immedOpnd range (e2, e1, ord)
    | commImmedOpnd range (e1 as T.CONST _, e2, ord) = 
       immedOpnd range (e2, e1, ord)
    | commImmedOpnd range (e1 as T.LABEL _, e2, ord) =
       immedOpnd range (e2, e1, ord)
    | commImmedOpnd range arg = immedOpnd range arg

  and eCommImmedOpnd range (oper, e1, e2, rt, ord) = 
   (case commImmedOpnd range (e1, e2, ord)
    of (ra, I.RegOp rb) =>
        emit(I.ARITH{oper=oper, ra=ra, rb=rb, rt=rt, Rc=false, OE=false})
     | (ra, opnd) => 
	emit(I.ARITHI{oper=oper, ra=ra, im=opnd, rt=rt})
    (*esac*))

  and orderedFF(fe1, fe2, T.LR) = (reduceFexp fe1, reduceFexp fe2)
    | orderedFF(fe1, fe2, T.RL) = let
        val f2 = reduceFexp fe2
      in (reduceFexp fe1, f2)
      end

  and reduceCCexpCd(ccexp, ccd) = 
   (case ccexp
     of T.CMP(cc, e1, e2, ord) => let
          val (opnds, cmp) =
	    (case cc
	      of (T.LT | T.LE | T.EQ | T.NEQ | T.GT | T.GE) =>
		   (immedOpnd signed16, I.CMP)
	       | _ => (immedOpnd unsigned16, I.CMPL)
	    (*esac*))
	  val (opndA, opndB) = opnds(e1, e2, ord)
        in emit(I.COMPARE{cmp=cmp, bf=ccd, ra=opndA, rb=opndB}); ccd
	end
      | T.FCMP(fcc, fe1, fe2, ord) => let
	  val (f1, f2) = orderedFF(fe1, fe2, ord)
        in emit(I.FCOMPARE{cmp=I.FCMPU, bf=ccd, fa=f1, fb=f2}); ccd
        end
      | T.CC cc => cc
      | _ => error "reduceCCexpCd: Not implemented"
   (*esac*))

  and reduceStm exp = let
  in 
    case exp of
       T.MV(rd, rexp) => let
         val rs = reduceExpRd(rexp, rd)
       in 
	 if rs=rd then () 
	 else emit(I.ARITH{oper=I.OR, rt=rd, ra=rs, rb=rs, Rc=false, OE=false})
       end
     | T.FMV(fd, fexp)  => let
         val fs = reduceFexpFd(fexp, fd)
       in 
	 if fs = fd then ()
	 else emit(I.FUNARY{oper=I.FMR, ft=fd, fb=fs, Rc=false})
       end
     | T.CCMV(ccd, ccexp) => let
         val ccs = reduceCCexpCd(ccexp, ccd)
       in 
	 if ccd = ccs then () else emit(I.MCRF{bf=ccd, bfa=ccs})
       end
     | T.COPY(dst, src) => 
         emit(I.COPY
	  {dst=dst, src=src, impl=ref NONE, 
	   tmp = case dst of [_] => NONE | _ => SOME(I.Direct(newReg()))})
     | T.FCOPY(dst, src) => 
         emit(I.FCOPY
	   {dst=dst, src=src, impl=ref NONE, 
	    tmp = case dst of [_] => NONE | _ => SOME(I.FDirect(newFreg()))})
     | T.JMP(T.LABEL lexp, labs) => 
	 emit(I.B{addr=I.LabelOp lexp, LK=false})
     | T.JMP(rexp,  labs) => let
	 val rs = reduceExp(rexp)
       in
	 emit(I.mtlr(rs));
	 emit(I.BCLR{bo=I.ALWAYS, bf=0, bit=I.LT, LK=false, labels=labs})
       end
     | T.CALL(rexp, defs, uses) => let
          val addCCreg = C.addCell C.CC
          fun live([],acc) = acc
            | live(T.GPR(T.REG r)::regs,acc) = live(regs, C.addReg(r,acc))
            | live(T.CCR(T.CC cc)::regs,acc) = live(regs, addCCreg(cc,acc))
            | live(T.FPR(T.FREG f)::regs,acc) = live(regs, C.addFreg(f,acc))
            | live(_::regs, acc) = live(regs, acc)
          val defs=live(defs,C.empty)
          val uses=live(uses,C.empty)
       in emit(I.mtlr(reduceExp rexp));
	  emit(I.CALL{def=defs, use=uses})
       end
     | T.RET => emit(I.ret())
     | T.STORE8(addr, data, mem) => store(I.Byte, addr, data, mem)
     | T.STORE32(addr, data, mem) => store(I.Word, addr, data, mem)
     | T.STORED(addr, data, mem) => let
         val (r, disp) = ea addr
       in emit(I.ST{sz=I.Double, rs=reduceFexp data, ra=r, d=disp, mem=mem})
       end
     | T.STORECC _ => error "STORECC: Not implemented"
     | T.BCC(_, T.CMP(_, T.LI _, T.LI _, _), _) => error "BCC"
     | T.BCC(cc, T.CMP(_, T.ANDB(e1, e2), T.LI 0, _), lab) => 
       (case commImmedOpnd unsigned16 (e1, e2, T.LR)
	 of (ra, I.RegOp rb) =>
	     emit(I.ARITH{oper=I.AND, ra=ra, rb=rb, rt=newReg(), Rc=true, OE=false})
	  | (ra, opnd) =>
	     emit(I.ARITHI{oper=I.AND, ra=ra, im=opnd, rt=newReg()})
	(*esac*);
	reduceStm(T.BCC(cc, T.CC 0, lab)))
     | T.BCC(cc, T.CMP(_, e1 as T.LI _, e2, ord), lab) => let
        val cc' = 
	  (case cc 
	    of T.LT => T.GT
 	     | T.LTU => T.GTU
	     | T.LE => T.GE
 	     | T.LEU => T.GEU
 	     | T.EQ => T.EQ
 	     | T.NEQ => T.NEQ
	     | T.GE => T.LE
 	     | T.GEU => T.LEU
 	     | T.GT => T.LT
 	     | T.GTU => T.LTU
	  (*esac*))
       in reduceStm(T.BCC(cc', T.CMP(cc', e2, e1, ord), lab)) 
       end
     | T.BCC(_, cmp as T.CMP(cond, _, _, _), lab) => let
         val ccreg = if true then 0 else newCCreg() (* XXX *)
	 val (bo, cf) = 
	   (case cond 
	     of	T.LT =>  (I.TRUE,  I.LT)
	      | T.LE =>  (I.FALSE, I.GT)
	      | T.EQ =>  (I.TRUE,  I.EQ)
 	      | T.NEQ => (I.FALSE, I.EQ)
	      | T.GT =>  (I.TRUE,  I.GT)
	      | T.GE =>  (I.FALSE, I.LT)
	      | T.LTU => (I.TRUE,  I.LT)
	      | T.LEU => (I.FALSE, I.GT)
	      | T.GTU => (I.TRUE,  I.GT)
	      | T.GEU => (I.FALSE, I.LT)
	    (*esac*))
	 val addr = I.LabelOp(LE.LABEL lab)
       in
	 reduceCCexpCd(cmp, ccreg);
	 emitBranch{bo=bo, bf=ccreg, bit=cf, addr=addr, LK=false}
       end
     | T.BCC(cc, T.CC cr, lab) => let
        val addr=I.LabelOp(LE.LABEL lab)
	fun branch(bo, bit) = 
	  emitBranch{bo=bo, bf=cr, bit=bit, addr=addr, LK=false}
       in
         case cc
	 of T.EQ  => branch(I.TRUE, I.EQ)
	  | T.NEQ => branch(I.FALSE, I.EQ)
	  | (T.LT | T.LTU) => branch(I.TRUE, I.LT)
          | (T.LE | T.LEU) => branch(I.FALSE, I.GT)
	  | (T.GE | T.GEU) => branch(I.FALSE, I.LT)
	  | (T.GT | T.GTU) => branch(I.TRUE, I.GT)
       end  
     | T.FBCC(_, cmp as T.FCMP(cond, _, _, _), lab) => let
         val ccreg = if true then 0 else newCCreg() (* XXX *)
	 val labOp = I.LabelOp(LE.LABEL lab)
	 fun branch(bo, bf, bit) = 
	   emitBranch{bo=bo, bf=bf, bit=bit, addr=labOp, LK=false}

	 fun test2bits(bit1, bit2) = let
	   val ba=(ccreg, bit1)
	   val bb=(ccreg, bit2)
	   val bt=(ccreg, I.FL)
         in
	   (emit(I.CCARITH{oper=I.CROR, bt=bt, ba=ba, bb=bb});
	    branch(I.TRUE, ccreg, I.FL))
         end
       in
	 reduceCCexpCd(cmp, ccreg);
	 case cond
	  of T.==  => branch(I.TRUE,  ccreg, I.FE)
           | T.?<> => branch(I.FALSE,  ccreg, I.FE)
	   | T.?   => branch(I.TRUE,  ccreg, I.FU)
	   | T.<=> => branch(I.FALSE,  ccreg, I.FU)
	   | T.>   => branch(I.TRUE,  ccreg, I.FG)
	   | T.>=  => test2bits(I.FG, I.FE)
	   | T.?>  => test2bits(I.FU, I.FG)
	   | T.?>= => branch(I.FALSE,  ccreg, I.FL)
	   | T.<   => branch(I.TRUE,  ccreg, I.FL)
	   | T.<=  => test2bits(I.FL, I.FE)
	   | T.?<  => test2bits(I.FU, I.FL)
	   | T.?<= => branch(I.FALSE,  ccreg, I.FG)
	   | T.<>  => test2bits(I.FL, I.FG)
	   | T.?=  => test2bits(I.FU, I.FE)
	 (*esac*)
       end
     | _ => error "reduceStm"
  end (* reduceStm *)

  and ea(T.ADD(e, T.LI i)) = let 
        val ra = reduceExp e
      in
       if ~32768 <= i andalso i < 32768 then (ra, I.ImmedOp i)
       else let 
	   val (hi, lo) = split i 
	   val tmpR = newReg()
	 in 
	   emit(I.ARITHI{oper=I.ADDS, rt=tmpR, ra=ra, im=I.ImmedOp hi});
	   (tmpR, I.ImmedOp lo)
	 end
      end
    | ea(T.ADD(T.LI i, e)) = ea(T.ADD(e, T.LI i))
    | ea(exp as T.SUB(e, T.LI i, _)) = 
        (ea(T.ADD(e, T.LI (~i))) 
	   handle Overflow => (reduceExp exp, I.ImmedOp 0))
    | ea(T.ADD(e1, e2)) = (reduceExp e1, I.RegOp (reduceExp e2))
    | ea e = (reduceExp e, I.ImmedOp 0)

  and store(sz, addr, data, mem) = let
    val (r, disp) = ea addr
  in emit(I.ST{sz=sz, rs=reduceExp data, ra=r, d=disp, mem=mem})
  end

  and subfImmed(i, ra, rt) = 
    emit(
      if signed16 i then
	 I.ARITHI{oper=I.SUBF, rt=rt, ra=ra, im=I.ImmedOp i}
      else
	 I.ARITH{oper=I.SUBF, rt=rt, ra=ra, rb=reduceExp(T.LI i), 
		 Rc=false, OE=false})


  and orderedRR(e1, e2, T.LR) = (reduceExp e1, reduceExp e2)
    | orderedRR(e1, e2, T.RL) = let
        val rb = reduceExp e2
      in (reduceExp e1, rb)
      end

  and arithTrapping(oper, e1, e2, rt, ord) = let
    val (ra, rb) = orderedRR(e1, e2, ord)
  in 
    if !trapsPresent = false then
     (trapsPresent:=true;
      trapLabel := SOME(Label.newLabel""))
    else ();
    emit(I.ARITH{oper=oper, ra=ra, rb=rb, rt=rt, OE=true, Rc=true});
    emitBranch{bo=I.TRUE, bf=0, bit=I.SO, LK=false,
	      addr=I.LabelOp(LE.LABEL(Option.valOf (!trapLabel)))}
  end
    
  and reduceExp(T.REG r) = r
    | reduceExp rexp = reduceExpRd(rexp, newReg())

  (* reduceExpRd(rexp, rd) -- reduce the expression rexp, giving 
   *	preference to register rd as the destination.
   *)
  and reduceExpRd(T.REG r, _) = r
    | reduceExpRd(T.SEQ(stm, e), rt) = (reduceStm stm; reduceExpRd(e, rt))
    | reduceExpRd(exp, rt) = 
      (case exp
       of T.LI i => loadImmed(i, rt)
	| T.LI32 w => let
	    val wtoi = Word32.toIntX
	  in
	   if w < 0w32768 then 
	     emit(I.ARITHI{oper=I.ADD, rt=rt, ra=0, im=I.ImmedOp (wtoi w)})
	   else let
	      val hi = Word32.~>>(w, 0w16)
	      val lo = Word32.andb(w, 0w65535)
	      val (high, low) = 
		if lo < 0w32768 then (hi, lo) else (hi+0w1, lo-0w65536)
	    in loadImmedHiLo(wtoi high, wtoi low, rt)
	    end
	  end
	| T.LABEL lexp => 
	   emit(I.ARITHI{oper=I.ADD, rt=rt, ra=0, im=I.LabelOp lexp})
	| T.CONST c =>
	   emit(I.ARITHI{oper=I.ADD, rt=rt, ra=0, im=I.ConstOp c})
	| T.ADD(e1, e2) => eCommImmedOpnd signed16 (I.ADD, e1, e2, rt, T.LR)
	| T.SUB(e1, e2 as T.LI i, ord) =>
	  ((reduceExpRd(T.ADD(e1, T.LI (~i)), rt); ())
	     handle Overflow => 
	      (emit(I.ARITH{oper=I.SUBF, rt=rt, ra=reduceExp e2, 
			    rb=reduceExp e1, OE=false, Rc=false})))

	| T.SUB(e1 as T.LI i, e2, ord) => subfImmed(i, reduceExp e2, rt)
	| T.SUB(e1, e2, ord) => 
	  (case e1
	    of T.CONST c => 
	         emit(I.ARITHI{oper=I.SUBF, rt=rt, ra=reduceExp e2, im=I.ConstOp c})
	     | T.LI32 w => subfImmed(Word32.toIntX w, reduceExp e2, rt)
	     | _ => let 
		   val (rb, ra) = orderedRR(e1, e2, ord)
		 in emit(I.ARITH{oper=I.SUBF, rt=rt, ra=ra, rb=rb, Rc=false, OE=false})
		 end
	  (*esac*))
	| T.MULU(e1, e2) => eCommImmedOpnd signed16 (I.MULL, e1, e2, rt, T.LR)
	| T.DIVU(e1, e2, ord) => arithTrapping(I.DIVWU, e1, e2, rt, ord)
	| T.ADDT(e1, e2) => arithTrapping(I.ADD, e1, e2, rt, T.LR)
	| T.SUBT(e1, e2, ord) => arithTrapping(I.SUBF, e2, e1, rt, ord)
	| T.MULT(e1, e2) => arithTrapping(I.MULL, e1, e2, rt, T.LR)
	| T.DIVT(e1, e2, ord) => arithTrapping(I.DIVW, e1, e2, rt, ord)

	| T.LOAD8(addr, mem) => let
	    val (r, disp) = ea addr
          in emit(I.L{sz=I.Byte, rt=rt, ra=r, d=disp, mem=mem})
          end
	| T.LOAD32(addr, mem) => let
	    val (r, disp) = ea addr
	  in emit(I.L{sz=I.Word, rt=rt, ra=r, d=disp, mem=mem})
	  end
	| T.ANDB(e1, e2) => eCommImmedOpnd unsigned16 (I.AND, e1, e2, rt, T.LR)

	| T.ORB(e1, e2) => eCommImmedOpnd unsigned16 (I.OR, e1, e2, rt, T.LR)
	| T.XORB(e1, e2) => eCommImmedOpnd unsigned16 (I.XOR, e1, e2, rt, T.LR)

	| T.SRA(e1, e2, ord) => shift (I.SRAW, e1, e2, rt, ord)
	| T.SRL(e1, e2, ord) => shift (I.SRW,  e1, e2, rt, ord)
	| T.SLL(e1, e2, ord) => shift (I.SLW,  e1, e2, rt, ord)
	| _ => error "reduceExpRd"
      (*esac*);
      rt)

  and shift(oper, e1, e2, rt, ord) = 
    emit
     (case immedOpnd unsigned16 (e1, e2, ord)
       of (opndA, I.RegOp rb) => 
	    I.ARITH{oper=oper, rt=rt, ra=opndA, rb=rb, Rc=false, OE=false}
	| (opndA, opndB) => 
	    I.ARITHI{oper=oper, rt=rt, ra=opndA, im=opndB}
      (*esac*))

  and reduceFexp(T.FREG f) = f
    | reduceFexp(e) = reduceFexpFd(e, newFreg())

  (* reduceFexpRd(fexp, fd) -- reduce the expression fexp, giving 
   *	preference to register fd as the destination.
   *)
  and reduceFexpFd(T.FREG f, _) = f
    | reduceFexpFd(T.FSEQ(stm, e), fd) = (reduceStm stm; reduceFexpFd(e, fd))
    | reduceFexpFd(fexp, fd) = let
       fun fbinary(oper, fe1, fe2, ord) = let
	 val (fa, fb) = orderedFF(fe1, fe2, ord)
       in emit(I.FARITH{oper=oper, fa=fa, fb=fb, ft=fd, Rc=false})
       end
       fun funary(oper, fe) = 
	 emit(I.FUNARY{oper=oper, ft=fd, fb=reduceFexp fe, Rc=false})
      in
       case fexp
	of T.LOADD(addr, mem) => let
	     val (r, opnd) = ea addr
	   in emit(I.L{sz=I.Double, rt=fd, ra=r, d=opnd, mem=mem})
	   end
         | T.FADDD(e1, e2) => fbinary(I.FADD, e1, e2, T.LR)
	 | T.FMULD(e1, e2) => fbinary(I.FMUL, e1, e2, T.LR)
	 | T.FSUBD(e1, e2, ord) => fbinary(I.FSUB, e1, e2, ord)
	 | T.FDIVD(e1, e2, ord) => fbinary(I.FDIV, e1, e2, ord)
	 | T.FABSD e => funary(I.FABS, e)
	 | T.FNEGD e => funary(I.FNEG, e)
	 | T.CVTI2D e => app emit (PseudoInstrs.cvti2d{reg=reduceExp e, fd=fd})
	 | _ => error "reduceFexpFd"
       (*esac*);
       fd
    end

  fun mltreeComp mltree = let
    fun emitTrap() = emit(I.TWI{to=31,ra=0,si=I.ImmedOp 0}) 
    fun mltc(T.PSEUDO_OP pOp)    = F.pseudoOp pOp
      | mltc(T.DEFINELABEL lab)  = F.defineLabel lab
      | mltc(T.ENTRYLABEL lab)   = F.entryLabel lab
      | mltc(T.ORDERED mlts)     = F.ordered mlts
      | mltc(T.BEGINCLUSTER)     = 
         (F.beginCluster();
	  trapLabel := NONE;
	  trapsPresent := false)
      | mltc(T.CODE stms)        = app reduceStm stms
      | mltc(T.BLOCK_NAME name)  = F.blockName name
      | mltc(T.ENDCLUSTER regmap)= 
	 (if !trapsPresent then
	    (F.defineLabel(Option.valOf (!trapLabel));
	     emitTrap();
	     trapsPresent := false;
	     trapLabel := NONE) 
	  else ();
	  F.endCluster regmap)
      | mltc(T.ESCAPEBLOCK regs) = F.exitBlock regs
  in mltc mltree
  end 

  fun mlriscComp stm = reduceStm stm
end

