functor SparcPseudoInstrs
   (Instr : SPARCINSTR where Region=CPSRegions) : SPARC_PSEUDO_INSTR = 
struct
  structure I = Instr
  structure C = Instr.C

  type reduceOpnd = I.operand -> int

  (* runtime system dependent *)
  val floatTmpOffset = I.IMMED 88
  val umulOffset = I.IMMED 80
  val smulOffset = I.IMMED 72
  val udivOffset = I.IMMED 84
  val sdivOffset = I.IMMED 76

  val stack = CPSRegions.stack

  val native = true  (* use native versions of the instructions? *)

  fun umul_native({r, i, d}, reduceOpnd) =
      [I.ARITH{a=I.UMUL,r=r,i=i,d=d,cc=false}]

      (* overflows iff Y != (d ~>> 31) *)
  fun smul_native({r, i, d}, reduceOpnd) =
      let val t1 = C.newReg()
          val t2 = C.newReg()
      in  [I.ARITH{a=I.SMUL,r=r,i=i,d=d,cc=false},
           I.SHIFT{s=I.SRA,r=d,i=I.IMMED 31,d=t1},
           I.RDY{d=t2},
           I.ARITH{a=I.SUB,r=t1,i=I.REG t2,d=0,cc=true},
           I.Ticc{t=I.BNE,r=0,i=I.IMMED 7}
          ] 
      end
  fun udiv_native({r,i,d},reduceOpnd) = 
      [I.WRY{r=0,i=I.REG 0},
       I.ARITH{a=I.UDIV,r=r,i=i,d=d,cc=false}]
  fun sdiv_native({r,i,d},reduceOpnd) = 
      let val t1 = C.newReg()
          val t2 = C.newReg()
          val t3 = C.newReg()
      in  [I.SETHI{i=0x200000, d=t1},
           I.ARITH{a=I.AND,r=t1,i=I.REG r,d=t2,cc=false},
           I.SHIFT{s=I.SRA,r=t2,i=I.IMMED 31,d=t3},
           I.WRY{r=t3,i=I.REG 0},
           I.ARITH{a=I.SDIV,r=r,i=i,d=d,cc=false}
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
       I.JMPL{r=addr,i=I.IMMED 0,d=C.linkReg,defs=defs,uses=uses,nop=true},
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

     (* Generate native versions of the instructions *)
  val umul = if native then umul_native else umul
  val smul = if native then smul_native else smul
  val udiv = if native then udiv_native else udiv
  val sdiv = if native then sdiv_native else sdiv

end

(*
 * $Log: sparcPseudoInstrs.sml,v $
 * Revision 1.1.1.1  1998/08/05 19:37:50  george
 *   Release 110.7.4
 *
 *)
