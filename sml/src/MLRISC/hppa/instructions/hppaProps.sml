(* hppaProps.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

functor HppaProps(HppaInstr : HPPAINSTR) : INSN_PROPERTIES = 
struct
  structure I = HppaInstr
  structure C = HppaInstr.C
  structure LE = I.LabelExp

  exception NegateConditional

  fun error msg = MLRiscErrorMsg.error("HppaProps",msg)

  datatype kind = IK_JUMP | IK_NOP | IK_INSTR | IK_COPY | IK_CALL 
                | IK_PHI | IK_SOURCE | IK_SINK
  datatype target = LABELLED of Label.label | FALLTHROUGH | ESCAPES

   (*========================================================================
    *  Instruction Kinds
    *========================================================================*)
  (* Note: BLE and BL used to implement calls are not view as branches *)
  fun instrKind(I.BCOND _)  = IK_JUMP
    | instrKind(I.BCONDI _) = IK_JUMP
    | instrKind(I.BB _)     = IK_JUMP
    | instrKind(I.B _)      = IK_JUMP
    | instrKind(I.BE _)     = IK_JUMP
    | instrKind(I.FBRANCH _)= IK_JUMP
    | instrKind(I.BV _)     = IK_JUMP
    | instrKind(I.BLR _)    = IK_JUMP
    | instrKind(I.NOP)      = IK_NOP
    | instrKind(I.COPY _)   = IK_COPY
    | instrKind(I.FCOPY _)  = IK_COPY
    | instrKind(I.BL  _)    = IK_CALL
    | instrKind(I.BLE _)    = IK_CALL
    | instrKind(I.PHI _)    = IK_PHI
    | instrKind(I.SOURCE _) = IK_SOURCE
    | instrKind(I.SINK _)   = IK_SINK
    | instrKind(I.ANNOTATION{i,...}) = instrKind i
    | instrKind _	    = IK_INSTR

  fun moveInstr(I.COPY _)   = true
    | moveInstr(I.FCOPY _)  = true
    | moveInstr(I.ANNOTATION{i,...}) = moveInstr i
    | moveInstr _ = false

  fun nop() = I.NOP

   (*========================================================================
    *  Parallel Move
    *========================================================================*)
  fun moveTmpR(I.COPY{tmp=SOME(I.Direct r), ...}) = SOME r
    | moveTmpR(I.FCOPY{tmp=SOME(I.FDirect f), ...}) = SOME f
    | moveTmpR(I.ANNOTATION{i,...}) = moveTmpR i
    | moveTmpR _ = NONE

  fun moveDstSrc(I.COPY{dst, src, ...}) = (dst, src)
    | moveDstSrc(I.FCOPY{dst, src, ...}) = (dst, src)
    | moveDstSrc(I.ANNOTATION{i,...}) = moveDstSrc i
    | moveDstSrc _ = error "moveDstSrc"

   (*========================================================================
    *  Branches and Calls/Returns
    *========================================================================*)
  fun branchTargets(I.BCOND{t, ...})    = [LABELLED t, FALLTHROUGH]
    | branchTargets(I.BCONDI{t, ...})   = [LABELLED t, FALLTHROUGH]
    | branchTargets(I.BB{t, ...})       = [LABELLED t, FALLTHROUGH]
    | branchTargets(I.B{lab, ...})      = [LABELLED lab]
    | branchTargets(I.FBRANCH{t,...})   = [LABELLED t, FALLTHROUGH]
    | branchTargets(I.BE{labs=[],...})  = [ESCAPES]
    | branchTargets(I.BE{labs,...})     = map LABELLED labs
    | branchTargets(I.BV{labs=[],...})  = [ESCAPES]
    | branchTargets(I.BV{labs,...})     = map LABELLED labs
    | branchTargets(I.BLR{labs,...})    = map LABELLED labs
    | branchTargets(I.ANNOTATION{i,...}) = branchTargets i
    | branchTargets _ = error "branchTargets"

  fun jump label = I.B{lab=label,n=true}

  val immedRange = {lo= ~8192, hi=8191}
  fun loadImmed{immed,t} = 
      I.LDO{i=if #lo immedRange <= immed andalso immed <= #hi immedRange 
              then I.IMMED immed
              else I.LabExp(LE.INT immed,I.F),b=0,t=t}
  fun loadOperand{opn,t} = I.LDO{i=opn,b=0,t=t}

  fun setTargets(I.BCOND{cmp,bc,r1,r2,t,f,n,nop},[F,T]) =
          I.BCOND{cmp=cmp,bc=bc,r1=r1,r2=r2,t=T,f=F,n=n,nop=nop}
    | setTargets(I.BCONDI{cmpi,bc,i,r2,t,f,n,nop=nop},[F,T]) =
          I.BCONDI{cmpi=cmpi,bc=bc,i=i,r2=r2,t=T,f=F,n=n,nop=nop}
    | setTargets(I.BB{bc,r,p,t,f,n,nop},[F,T]) =
          I.BB{bc=bc,r=r,p=p,t=T,f=F,n=n,nop=nop}
    | setTargets(I.B{n,...},[L]) = I.B{lab=L,n=n}
    | setTargets(I.FBRANCH{cc,fmt,n,long,f1,f2,...},[F,T]) =
          I.FBRANCH{cc=cc,fmt=fmt,t=T,f=F,n=n,long=long,f1=f1,f2=f2} 
    | setTargets(I.BV{x,b,n,...},labels) = I.BV{x=x,b=b,labs=labels,n=n}
    | setTargets(I.BE{b,d,n,sr,...},labs) = I.BE{b=b,d=d,n=n,sr=sr,labs=labs}
    | setTargets(I.BLR{x,t,n,...},labels) = I.BLR{x=x,t=t,labs=labels,n=n}
    | setTargets(I.ANNOTATION{i,a},labels) =
          I.ANNOTATION{i=setTargets(i,labels),a=a}
    | setTargets(i,_) = i

  fun negateConditional br = let
    fun revFcond I.?    = I.!?
      | revFcond I.!<=> = I.<=>
      | revFcond I.==   = I.!=
      | revFcond I.?=   = I.!?=
      | revFcond I.!<>  = I.<>
      | revFcond I.!?>= = I.?>=
      | revFcond I.<    = I.!<
      | revFcond I.?<   = I.!?<
      | revFcond I.!>=  = I.>=
      | revFcond I.!?>  = I.?>
      | revFcond I.<=   = I.!<=
      | revFcond I.?<=  = I.!?<=
      | revFcond I.!>   = I.>
      | revFcond I.!?<= = I.?<=
      | revFcond I.>    = I.!>
      | revFcond I.?>   = I.!?>
      | revFcond I.!<=  = I.<=
      | revFcond I.!?<  = I.?<
      | revFcond I.>=   = I.!>=
      | revFcond I.?>=  = I.!?>=
      | revFcond I.!<   = I.<
      | revFcond I.!?=  = I.?=
      | revFcond I.<>   = I.!<>
      | revFcond I.!=   = I.==
      | revFcond I.!?   = I.?
      | revFcond I.<=>  = I.!<=>
  in
    case br of 
      I.BCOND{cmp,bc,r1,r2,t,f,n,nop} => 
         I.BCOND{bc=bc, r1=r1, r2=r2, t=t, f=f, n=n, nop=nop,
		 cmp=case cmp of I.COMBT => I.COMBF | I.COMBF => I.COMBT}
    | I.BCONDI{cmpi,bc,i,r2,t,f,n,nop} =>
        I.BCONDI{bc=bc, i=i, r2=r2, t=t, f=f, n=n, nop=nop,
		 cmpi=case cmpi of I.COMIBT => I.COMIBF | I.COMIBF => I.COMIBT}
    | I.BB{bc,r,p,t,f,n,nop} => 
         I.BB{bc=case bc of I.BSET => I.BCLR | I.BCLR => I.BSET, 
              r=r,p=p,t=t,f=f,n=n,nop=nop}
    | I.FBRANCH{cc,fmt,f1,f2,t,f,n,long} =>
        I.FBRANCH{cc=revFcond cc,fmt=fmt,f1=f1,f2=f2,t=t,f=f,n=n,long=long} 
    | I.ANNOTATION{i,a} => I.ANNOTATION{i=negateConditional i,a=a}
    | _ => raise NegateConditional
  end

  (*========================================================================
   *  Equality and hashing for operands
   *========================================================================*)
   fun hashFieldSel I.F = 0w0
     | hashFieldSel I.S = 0w1
     | hashFieldSel I.D = 0w2
     | hashFieldSel I.R = 0w3
     | hashFieldSel I.T = 0w4
     | hashFieldSel I.P = 0w5
   fun hashOpn(I.IMMED i) = Word.fromInt i
     | hashOpn(I.LabExp(l,f)) = I.LabelExp.hash l + hashFieldSel f
     | hashOpn(I.HILabExp(l,f)) = I.LabelExp.hash l + hashFieldSel f + 0w10000
     | hashOpn(I.LOLabExp(l,f)) = I.LabelExp.hash l + hashFieldSel f + 0w20000
   fun eqOpn(I.IMMED i,I.IMMED j) = i = j
     | eqOpn(I.LabExp(a,b),I.LabExp(c,d)) = b = d andalso I.LabelExp.==(a,c)
     | eqOpn(I.HILabExp(a,b),I.HILabExp(c,d)) = b = d andalso I.LabelExp.==(a,c)
     | eqOpn(I.LOLabExp(a,b),I.LOLabExp(c,d)) = b = d andalso I.LabelExp.==(a,c)
     | eqOpn _ = false
   

  (*========================================================================
   *  Definition and use (for register allocation mainly)
   *========================================================================*)
  fun defUseR instr = let
    fun trap((I.ADDO | I.SUBO | I.SH1ADDO), d, u) = (d, u)
      | trap(_, d, u) = (d, u)
    fun trapi((I.ADDIO | I.SUBIO), d, u) = (d, u)
      | trapi(_, d, u) = (d, u)
  in
    case instr
     of I.STORE {b, r,...}          => ([],  [b,r])
      | I.LOAD {l, r1, r2, t, ...}  => ([t], [r1,r2])
      | I.LOADI {li, r, t, ...}     => ([t], [r])
      | I.ARITH {a, r1, r2, t, ...} => trap(a, [t], [r1,r2])
      | I.ARITHI {ai, r, t, ...}    => trapi(ai, [t], [r])
      | I.COMCLR_LDO{r1, r2, b, t1, t2, ...}=> 
          if t1 = t2 then ([t1], [b, r1, r2])
          else ([t1, t2], [b, r1, r2, t2])
      | I.COMICLR_LDO{i1, r2, b, t1, t2, ...}=> 
          if t1 = t2 then ([t1], [b, r2])
          else ([t1, t2], [b, r2, t2])
      | I.SHIFTV {r, t, ...}        => ([t], [r])
      | I.SHIFT {r, t, ...}         => ([t], [r])
      | I.BCOND {r1, r2, ...}       => ([],  [r1,r2])
      | I.BCONDI {r2, ...} 	    => ([],  [r2])
      | I.BB {r, ...} 	            => ([],  [r])
      | I.BV {x, b, ...}	    => ([],  [x,b])
      | I.BE {b, ...}	            => ([],  [b])
      | I.BLR{x, t, ...}            => ([t], [x])
      | I.BL{defs, uses, ...}       => (#1 defs, #1 uses)
      | I.BLE{t, b, defs, uses, ...}=> (31 :: t :: #1 defs, b :: #1 uses)
      | I.LDIL{i, t}		    => ([t], [])
      | I.LDO{b, t, ...}	    => ([t], [b])
      | I.COPY{dst, src, tmp=SOME(I.Direct r), ...} => (r::dst, src)
      | I.COPY{dst, src, ...}       => (dst, src)
      | I.MTCTL{r, t}		    => ([],  [r])
      | I.FSTORE {b, ...}	    => ([],  [b])
      | I.FSTOREX {b, x, ...}  	    => ([],  [b,x])
      | I.FLOAD {b, ...}	    => ([],  [b])
      | I.FLOADX{b, x, ...} 	    => ([],  [b,x])
      | I.ANNOTATION{a=C.DEF_USE{cellkind=C.GP,defs,uses}, i, ...} => 
        let val (d,u) = defUseR i in (defs@d, u@uses) end
      | I.ANNOTATION{a, i, ...} => defUseR i
      | _   => ([],[])
  end

  fun defUseF instr = 
    case instr
      of I.FSTORE {r, ...}  	   => ([],  [r])
       | I.FSTOREX{r, ...}	   => ([],  [r])
       | I.FLOAD{t, ...}	   => ([t], [])
       | I.FLOADX{t, ...}	   => ([t], [])
       | I.FARITH {r1, r2, t, ...} => ([t], [r1,r2])
       | I.FUNARY {f, t, ...}      => ([t], [f])
       | I.FCNV {f, t, ...}        => ([t], [f])
       | I.FBRANCH{f1, f2,...}	   => ([],  [f1, f2])
       | I.BL{defs, uses, ...}     => (#2 defs, #2 uses)
       | I.BLE{defs, uses, ...}    => (#2 defs, #2 uses)
       | I.FCOPY{dst, src, tmp=SOME(I.FDirect f), ...} => (f::dst, src)
       | I.FCOPY{dst, src, ...}    => (dst, src)
       | I.ANNOTATION{a=C.DEF_USE{cellkind=C.FP,defs,uses}, i, ...} => 
         let val (d,u) = defUseF i in (defs@d, u@uses) end
       | I.ANNOTATION{a, i, ...} => defUseF i
       | _ => ([],[])

  fun defUse C.GP = defUseR
    | defUse C.FP = defUseF
    | defUse _ = error "defUse"

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



