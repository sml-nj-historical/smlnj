functor SparcProps(SparcInstr : SPARCINSTR) : INSN_PROPERTIES =
struct
  structure I = SparcInstr
  structure C = I.C
  structure T = I.T 
  structure LE = I.LabelExp 

  exception NegateConditional

  fun error msg = MLRiscErrorMsg.error("SparcProps",msg)

  datatype kind = IK_JUMP | IK_NOP | IK_INSTR | IK_COPY | IK_CALL 
                | IK_CALL_WITH_CUTS | IK_PHI | IK_SOURCE | IK_SINK
  datatype target = LABELLED of Label.label | FALLTHROUGH | ESCAPES

  val zeroR = Option.valOf(C.zeroReg C.GP)
  val r15   = C.Reg C.GP 15
  val r31   = C.Reg C.GP 31

  (*========================================================================
   *  Instruction Kinds
   *========================================================================*)
  fun instrKind(I.Bicc _)  = IK_JUMP
    | instrKind(I.FBfcc _) = IK_JUMP
    | instrKind(I.JMP _)   = IK_JUMP
    | instrKind(I.RET _)   = IK_JUMP
    | instrKind(I.BR _)    = IK_JUMP
    | instrKind(I.BP _)    = IK_JUMP
    | instrKind(I.COPY _)  = IK_COPY
    | instrKind(I.FCOPY _) = IK_COPY
    | instrKind(I.CALL{cutsTo=_::_,...})  = IK_CALL_WITH_CUTS
    | instrKind(I.CALL _)  = IK_CALL
    | instrKind(I.JMPL{cutsTo=_::_,...})  = IK_CALL_WITH_CUTS
    | instrKind(I.JMPL _)  = IK_CALL
    | instrKind(I.PHI _)    = IK_PHI
    | instrKind(I.SOURCE _) = IK_SOURCE
    | instrKind(I.SINK _)   = IK_SINK
    | instrKind(I.ANNOTATION{i,...}) = instrKind i
    | instrKind _          = IK_INSTR

  fun branchTargets(I.Bicc{b=I.BA,label,...}) = [LABELLED label]
    | branchTargets(I.Bicc{label,...}) = [LABELLED label, FALLTHROUGH] 
    | branchTargets(I.FBfcc{b=I.FBA,label,...}) = [LABELLED label]
    | branchTargets(I.FBfcc{label,...}) = [LABELLED label, FALLTHROUGH]
    | branchTargets(I.BR{label,...}) = [LABELLED label, FALLTHROUGH]
    | branchTargets(I.BP{label,...}) = [LABELLED label, FALLTHROUGH]
    | branchTargets(I.JMP{labs=[],...}) = [ESCAPES] 
    | branchTargets(I.RET _)   = [ESCAPES]
    | branchTargets(I.JMP{labs,...})    = map LABELLED labs
    | branchTargets(I.CALL{cutsTo,...}) = FALLTHROUGH::map LABELLED cutsTo
    | branchTargets(I.JMPL{cutsTo,...}) = FALLTHROUGH::map LABELLED cutsTo
    | branchTargets(I.ANNOTATION{i,...}) = branchTargets i
    | branchTargets _ = error "branchTargets"

  fun setTargets(I.Bicc{b=I.BA,a,nop,...},[L]) = 
          I.Bicc{b=I.BA,a=a,label=L,nop=nop}
    | setTargets(I.Bicc{b,a,nop,...},[F,T]) = 
          I.Bicc{b=b,a=a,label=T,nop=nop}
    | setTargets(I.FBfcc{b,a,nop,...},[F,T]) = 
          I.FBfcc{b=b,a=a,label=T,nop=nop}
    | setTargets(I.BR{rcond,p,r,a,nop,...},[F,T]) = 
          I.BR{rcond=rcond,p=p,r=r,a=a,label=T,nop=nop}
    | setTargets(I.BP{b,cc,p,a,nop,...},[F,T]) = 
          I.BP{b=b,cc=cc,p=p,a=a,label=T,nop=nop}
    | setTargets(I.JMP{r,i,nop,...},labels) = 
          I.JMP{r=r,i=i,labs=labels,nop=nop}
    | setTargets(I.ANNOTATION{i,a},labs) = 
          I.ANNOTATION{i=setTargets(i,labs),a=a}
    | setTargets(i,_) = i

   fun revCond I.BA = I.BN
     | revCond I.BN = I.BA
     | revCond I.BNE = I.BE
     | revCond I.BE  = I.BNE
     | revCond I.BG  = I.BLE
     | revCond I.BLE = I.BG
     | revCond I.BGE = I.BL
     | revCond I.BL  = I.BGE
     | revCond I.BGU = I.BLEU
     | revCond I.BLEU = I.BGU
     | revCond I.BCC  = I.BCS
     | revCond I.BCS  = I.BCC
     | revCond I.BPOS = I.BNEG
     | revCond I.BNEG = I.BPOS
     | revCond I.BVC  = I.BVS
     | revCond I.BVS  = I.BVC

   fun revFcond I.FBA   = I.FBN
     | revFcond I.FBN   = I.FBA
     | revFcond I.FBU   = I.FBO
     | revFcond I.FBG   = I.FBULE
     | revFcond I.FBUG  = I.FBLE
     | revFcond I.FBL   = I.FBUGE
     | revFcond I.FBUL  = I.FBGE
     | revFcond I.FBLG  = I.FBUE
     | revFcond I.FBNE  = I.FBE
     | revFcond I.FBE   = I.FBNE
     | revFcond I.FBUE  = I.FBLG
     | revFcond I.FBGE  = I.FBUL
     | revFcond I.FBUGE = I.FBL
     | revFcond I.FBLE  = I.FBUG
     | revFcond I.FBULE = I.FBG
     | revFcond I.FBO   = I.FBU

  fun revRcond I.RZ   = I.RNZ
    | revRcond I.RLEZ = I.RGZ
    | revRcond I.RLZ  = I.RGEZ
    | revRcond I.RNZ  = I.RZ
    | revRcond I.RGZ  = I.RLEZ
    | revRcond I.RGEZ = I.RLZ

  fun revP I.PT = I.PN
    | revP I.PN = I.PT

  fun negateConditional(I.Bicc{b,a,label,nop}) =
         I.Bicc{b=revCond b,a=a,label=label,nop=nop}
    | negateConditional(I.FBfcc{b,a,label,nop}) =
         I.FBfcc{b=revFcond b,a=a,label=label,nop=nop} 
    | negateConditional(I.BR{p,r,rcond,a,label,nop}) =
         I.BR{p=revP p,a=a,r=r,rcond=revRcond rcond,label=label,nop=nop} 
    | negateConditional(I.BP{b,cc,p,a,label,nop}) =
         I.BP{p=revP p,a=a,b=revCond b,cc=cc,label=label,nop=nop} 
    | negateConditional(I.ANNOTATION{i,a}) = 
         I.ANNOTATION{i=negateConditional i,a=a}
    | negateConditional _ = raise NegateConditional

  fun jump label = I.Bicc{b=I.BA,a=true,label=label,nop=true}

  val immedRange = {lo= ~4096, hi = 4095}

  fun loadImmed{immed,t} = 
      I.ARITH{a=I.OR,r=zeroR,i=
              if #lo immedRange <= immed andalso immed <= #hi immedRange 
              then I.IMMED immed else I.LAB(T.LI(IntInf.fromInt immed)),d=t}
  fun loadOperand{opn, t} = I.ARITH{a=I.OR,r=zeroR,i=opn, d=t}

  fun moveInstr(I.COPY _)  = true
    | moveInstr(I.FCOPY _) = true
    | moveInstr(I.ANNOTATION{i,...}) = moveInstr i
    | moveInstr _          = false

  fun nop() = I.SETHI{d=zeroR, i=0}

  (*========================================================================
   *  Parallel Move
   *========================================================================*)
  fun moveTmpR(I.COPY{tmp=SOME(I.Direct r),...}) = SOME r
    | moveTmpR(I.FCOPY{tmp=SOME(I.FDirect f),...}) = SOME f
    | moveTmpR(I.ANNOTATION{i,...}) = moveTmpR i
    | moveTmpR _ = NONE

  fun moveDstSrc(I.COPY{dst,src,...}) = (dst,src)
    | moveDstSrc(I.FCOPY{dst,src,...}) = (dst,src)
    | moveDstSrc(I.ANNOTATION{i,...}) = moveDstSrc i
    | moveDstSrc _ = error "moveDstSrc"

  (*========================================================================
   *  Equality and hashing
   *========================================================================*)
   fun hashOpn(I.REG r) = C.hashCell r
     | hashOpn(I.IMMED i) = Word.fromInt i
     | hashOpn(I.LAB l) = LE.hash l
     | hashOpn(I.LO l) = LE.hash l
     | hashOpn(I.HI l) = LE.hash l
   fun eqOpn(I.REG a,I.REG b) = C.sameColor(a,b)
     | eqOpn(I.IMMED a,I.IMMED b) = a = b
     | eqOpn(I.LAB a,I.LAB b) = LE.==(a,b)
     | eqOpn(I.LO a,I.LO b) = LE.==(a,b)
     | eqOpn(I.HI a,I.HI b) = LE.==(a,b)
     | eqOpn _ = false

  fun defUseR instr =
    let
       fun oper (I.REG r,def,use) = (def,r::use)
         | oper (_,def,use)       = (def,use)
    in
	case instr of
	  (* load/store instructions *)
          I.LOAD {r,d,i,...} => oper(i,[d],[r])
        | I.STORE {r,d,i,...} => oper(i,[],[r,d])
        | I.FLOAD {r,d,i,...} => oper(i,[],[r])
        | I.FSTORE {r,d,i,...} => oper(i,[],[r])
        | I.SETHI {d,...} => ([d],[])
        | I.ARITH {r,i,d,...} => oper(i,[d],[r])
        | I.SHIFT {r,i,d,...} => oper(i,[d],[r])
        | I.JMPL{defs,uses,d,r,i,...} => 
             oper(i,d:: C.getReg defs,r:: C.getReg uses)
        | I.BR{r,...} => ([],[r])
        | I.MOVicc{i,d,...} => oper(i,[d],[d])
        | I.MOVfcc{i,d,...} => oper(i,[d],[d])
        | I.MOVR{r,i,d,...} => oper(i,[d],[r,d])
        | I.CALL{defs,uses,...} => (r15 :: C.getReg defs, C.getReg uses)
        | I.JMP{r,i,...} => oper(i,[],[r])
        | I.RET{leaf=false,...} => ([],[r31])
        | I.RET{leaf=true,...} => ([],[r15])
        | I.COPY{src,dst,tmp=SOME(I.Direct r),...} => (r::dst,src)
        | I.COPY{src,dst,...} => (dst,src)
        | I.SAVE{r,i,d} => oper(i,[d],[r])
        | I.RESTORE{r,i,d} => oper(i,[d],[r])
        | I.Ticc{r,i,...} => oper(i,[],[r]) 
        | I.RDY{d,...} => ([d],[]) 
        | I.WRY{r,i,...} => oper(i,[],[r]) 
        | I.ANNOTATION{a=C.DEF_USE{cellkind=C.GP,defs,uses}, i, ...} => 
          let val (d,u) = defUseR i in (defs@d, u@uses) end
        | I.ANNOTATION{a, i, ...} => defUseR i
        | _ => ([],[])  
    end

  (* Use of FP registers *)
  fun defUseF instr =
      case instr of
        I.FLOAD{r,d,i,...} => ([d],[])
      | I.FSTORE{r,d,i,...} => ([],[d])
      | I.FPop1{r,d,...} => ([d],[r])
      | I.FPop2{r1,r2,d,...} => ([d],[r1,r2])
      | I.FCMP{r1,r2,...} => ([],[r1,r2])
      | I.JMPL{defs,uses,...} => (C.getFreg defs,C.getFreg uses)
      | I.CALL{defs,uses,...} => (C.getFreg defs,C.getFreg uses)
      | I.FMOVicc{r,d,...} => ([d],[r,d])
      | I.FMOVfcc{r,d,...} => ([d],[r,d])
      | I.FCOPY{src,dst,tmp=SOME(I.FDirect r),...} => (r::dst,src)
      | I.FCOPY{src,dst,...} => (dst,src)
      | I.ANNOTATION{a=C.DEF_USE{cellkind=C.FP,defs,uses}, i, ...} => 
        let val (d,u) = defUseF i in (defs@d, u@uses) end
      | I.ANNOTATION{a, i, ...} => defUseF i
      | _ => ([],[])

  fun defUse C.GP = defUseR
    | defUse C.FP = defUseF
    | defUse _    = error "defUse"

  (*========================================================================
   *  Annotations 
   *========================================================================*)
  fun getAnnotations(I.ANNOTATION{i,a}) = 
       let val (i,an) = getAnnotations i in (i,a::an) end
    | getAnnotations i = (i,[])
  fun annotate(i,a) = I.ANNOTATION{i=i,a=a}

  (*========================================================================
   *  Replicate an instruction
   *========================================================================*)
  fun replicate(I.ANNOTATION{i,a}) = I.ANNOTATION{i=replicate i,a=a}
    | replicate(I.COPY{tmp=SOME _, dst, src, impl}) =  
        I.COPY{tmp=SOME(I.Direct(C.newReg())), dst=dst, src=src, impl=ref NONE}
    | replicate(I.FCOPY{tmp=SOME _, dst, src, impl}) = 
        I.FCOPY{tmp=SOME(I.FDirect(C.newFreg())), 
                dst=dst, src=src, impl=ref NONE}
    | replicate i = i
end
