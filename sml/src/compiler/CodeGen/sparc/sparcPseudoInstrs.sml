functor SparcPseudoInstrs
   (Instr : SPARCINSTR where Region=CPSRegions) : SPARC_PSEUDO_INSTR = 
struct
  structure I = Instr
  structure C = Instr.C

  type format1 =
       {r:int, i:I.operand, d:int} *
       (I.operand -> I.C.register) -> I.instruction list

  type format2 =
       {i:I.operand, d:int} *
       (I.operand -> I.C.register) -> I.instruction list

  fun error msg = MLRiscErrorMsg.impossible ("SparcPseudoInstrs."^msg)

  (* runtime system dependent *)
  val floatTmpOffset = I.IMMED 88
  val umulOffset = I.IMMED 80
  val smulOffset = I.IMMED 72
  val udivOffset = I.IMMED 84
  val sdivOffset = I.IMMED 76

  val stack = CPSRegions.stack

  val native = true  (* use native versions of the instructions? *)

  fun umul_native({r, i, d}, reduceOpnd) =
      [I.ARITH{a=I.UMUL,r=r,i=i,d=d}]

  val TNE = I.Ticc{t=I.BNE,cc=I.ICC,r=0,i=I.IMMED 7}
  val TVS = I.Ticc{t=I.BVS,cc=I.ICC,r=0,i=I.IMMED 7}

      (* overflows iff Y != (d ~>> 31) *)
  fun smul_native({r, i, d}, reduceOpnd) =
      let val t1 = C.newReg()
          val t2 = C.newReg()
      in  [I.ARITH{a=I.SMUL,r=r,i=i,d=d},
           I.SHIFT{s=I.SRA,r=d,i=I.IMMED 31,d=t1},
           I.RDY{d=t2},
           I.ARITH{a=I.SUBCC,r=t1,i=I.REG t2,d=0},
           TNE
          ] 
      end
  fun udiv_native({r,i,d},reduceOpnd) = 
      [I.WRY{r=0,i=I.REG 0},
       I.ARITH{a=I.UDIV,r=r,i=i,d=d}]

   (* May overflow if MININT div -1 *)
  fun sdiv_native({r,i,d},reduceOpnd) = 
      let val t1 = C.newReg()
      in  [I.SHIFT{s=I.SRA,r=r,i=I.IMMED 31,d=t1},
           I.WRY{r=t1,i=I.REG 0},
           I.ARITH{a=I.SDIVCC,r=r,i=i,d=d},
           TVS
          ]
      end

  (* 
   * Registers %o2, %o3 are used to pass arguments to ml_mul and ml_div 
   * Result is returned in %o2.
   *)

  fun callRoutine(offset,reduceOpnd,r,i,d) =   
  let val addr = C.newReg()
      val defs = C.addReg(10,C.empty) 
      val uses = C.addReg(10,C.addReg(11,C.empty))
  in
      [I.COPY{src=[r,reduceOpnd i],dst=[10,11],
                   tmp=SOME(I.Direct(C.newReg())),impl=ref NONE},
       I.LOAD{l=I.LD,r=C.stackptrR,i=offset,d=addr,mem=stack},
       I.JMPL{r=addr,i=I.IMMED 0,d=C.linkReg,defs=defs,uses=uses,nop=true,mem=stack},
       I.COPY{src=[10],dst=[d],tmp=NONE,impl=ref NONE}
      ]
  end

  fun umul({r, i, d}, reduceOpnd) = callRoutine(umulOffset,reduceOpnd,r,i,d)
  fun smul({r, i, d}, reduceOpnd) = callRoutine(smulOffset,reduceOpnd,r,i,d)
  fun udiv({r, i, d}, reduceOpnd) = callRoutine(udivOffset,reduceOpnd,r,i,d)
  fun sdiv({r, i, d}, reduceOpnd) = callRoutine(sdivOffset,reduceOpnd,r,i,d)

  fun cvti2d({i, d}, reduceOpnd) = 
      [I.STORE{s=I.ST,r=C.stackptrR,i=floatTmpOffset,d=reduceOpnd i,mem=stack},
       I.FLOAD{l=I.LDF,r=C.stackptrR,i=floatTmpOffset,d=d,mem=stack},
       I.FPop1{a=I.FiTOd,r=d,d=d}
      ]
  fun cvti2s _ = error "cvti2s"
  fun cvti2q _ = error "cvti2q"

     (* Generate native versions of the instructions *)
  val umul = if native then umul_native else umul
  val smul = if native then smul_native else smul
  val udiv = if native then udiv_native else udiv
  val sdiv = if native then sdiv_native else sdiv

  val overflowtrap32 = (* tvs 0x7 *)
                       [I.Ticc{t=I.BVS,cc=I.ICC,r=0,i=I.IMMED 7}]
  val overflowtrap64 = [] (* not needed *)


end

(*
 * $Log$
 *)
