functor SparcPseudoInstrs
   (Instr : SPARCINSTR where Region=CPSRegions) : SPARC_PSEUDO_INSTR = 
struct
  structure I = Instr
  structure C = Instr.C

  type format1 =
       {r:CellsBasis.cell, i:I.operand, d:CellsBasis.cell} *
       (I.operand -> CellsBasis.cell) -> I.instruction list

  type format2 =
       {i:I.operand, d:CellsBasis.cell} *
       (I.operand -> CellsBasis.cell) -> I.instruction list

  fun error msg = MLRiscErrorMsg.impossible ("SparcPseudoInstrs."^msg)

  val delta = SparcSpec.framesize	(* initial value of %fp - %sp *)

  (* runtime system dependent; the numbers are relative to %sp but
   * we need offsets relative to %fp, hence the adjustment by delta *)
  val floatTmpOffset = I.IMMED (88 - delta)
  val umulOffset = I.IMMED (80 - delta)
  val smulOffset = I.IMMED (72 - delta)
  val udivOffset = I.IMMED (84 - delta)
  val sdivOffset = I.IMMED (76 - delta)

  val stack = CPSRegions.stack

  val native = true  (* use native versions of the instructions? *)

  fun umul_native({r, i, d}, reduceOpnd) =
      [I.ARITH{a=I.UMUL,r=r,i=i,d=d}]

  val TNE = I.Ticc{t=I.BNE,cc=I.ICC,r=C.r0,i=I.IMMED 7}
  val TVS = I.Ticc{t=I.BVS,cc=I.ICC,r=C.r0,i=I.IMMED 7}

      (* overflows iff Y != (d ~>> 31) *)
  fun smul_native({r, i, d}, reduceOpnd) =
      let val t1 = C.newReg()
          val t2 = C.newReg()
      in  [I.ARITH{a=I.SMUL,r=r,i=i,d=d},
           I.SHIFT{s=I.SRA,r=d,i=I.IMMED 31,d=t1},
           I.RDY{d=t2},
           I.ARITH{a=I.SUBCC,r=t1,i=I.REG t2,d=C.r0},
           TNE
          ] 
      end
  fun udiv_native({r,i,d},reduceOpnd) = 
      [I.WRY{r=C.r0,i=I.REG C.r0},
       I.ARITH{a=I.UDIV,r=r,i=i,d=d}]

   (* May overflow if MININT div -1 *)
  fun sdiv_native({r,i,d},reduceOpnd) = 
      let val t1 = C.newReg()
      in  [I.SHIFT{s=I.SRA,r=r,i=I.IMMED 31,d=t1},
           I.WRY{r=t1,i=I.REG C.r0},
           I.ARITH{a=I.SDIVCC,r=r,i=i,d=d},
           TVS
          ]
      end

  (* 
   * Registers %o2, %o3 are used to pass arguments to ml_mul and ml_div 
   * Result is returned in %o2.
   *)
  val r10 = C.GPReg 10
  val r11 = C.GPReg 11

  fun callRoutine(offset,reduceOpnd,r,i,d) =   
  let val addr = C.newReg()
      val defs = C.addReg(r10,C.empty) 
      val uses = C.addReg(r10,C.addReg(r11,C.empty))
  in
      [I.COPY{src=[r,reduceOpnd i],dst=[r10,r11],
                   tmp=SOME(I.Direct(C.newReg())),impl=ref NONE},
       I.LOAD{l=I.LD,r=C.frameptrR,i=offset,d=addr,mem=stack},
       I.JMPL{r=addr,i=I.IMMED 0,d=C.linkReg,defs=defs,uses=uses,
              cutsTo=[],nop=true,mem=stack},
       I.COPY{src=[r10],dst=[d],tmp=NONE,impl=ref NONE}
      ]
  end

  fun umul({r, i, d}, reduceOpnd) = callRoutine(umulOffset,reduceOpnd,r,i,d)
  fun smultrap({r, i, d}, reduceOpnd) = callRoutine(smulOffset,reduceOpnd,r,i,d)
  fun udiv({r, i, d}, reduceOpnd) = callRoutine(udivOffset,reduceOpnd,r,i,d)
  fun sdivtrap({r, i, d}, reduceOpnd) = callRoutine(sdivOffset,reduceOpnd,r,i,d)

  fun cvti2d({i, d}, reduceOpnd) = 
      [I.STORE{s=I.ST,r=C.frameptrR,i=floatTmpOffset,d=reduceOpnd i,mem=stack},
       I.FLOAD{l=I.LDF,r=C.frameptrR,i=floatTmpOffset,d=d,mem=stack},
       I.FPop1{a=I.FiTOd,r=d,d=d}
      ]
  fun cvti2s _ = error "cvti2s"
  fun cvti2q _ = error "cvti2q"

     (* Generate native versions of the instructions *)
  val umul32 = if native then umul_native else umul
  fun smul32 _ = error "smul32"
  val smul32trap = if native then smul_native else smultrap
  val udiv32 = if native then udiv_native else udiv
  fun sdiv32 _ = error "sdiv32"
  val sdiv32trap = if native then sdiv_native else sdivtrap

  val overflowtrap32 = (* tvs 0x7 *)
                       [I.Ticc{t=I.BVS,cc=I.ICC,r=C.r0,i=I.IMMED 7}]
  val overflowtrap64 = [] (* not needed *)


end

