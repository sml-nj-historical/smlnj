functor PPCPseudoInstr
  (structure Instr : PPCINSTR
     where Region = CPSRegions) : PPC_PSEUDO_INSTR = 
struct
  structure I = Instr
  structure C = Instr.C

  val stack = CPSRegions.stack
  val cvti2dTmpOff = 16			(* runtime system dependent *)
  val cvti2dConstOff = 8		(*            ''             *)
  val sp = C.stackptrR

  (* Cute little trick -- go figure *)
  fun cvti2d{reg, fd} = let
    val tmpR = C.newReg()
    val tmpF = C.newFreg()
  in
    [I.ARITHI{oper=I.XORS, rt=tmpR, ra=reg, im=I.ImmedOp 32768},
     I.ST{sz=I.Word, rs=tmpR, ra=sp, d=I.ImmedOp(cvti2dTmpOff+4), mem=stack},
     I.ARITHI{oper=I.ADDS, rt=tmpR, ra=0, im=I.ImmedOp(0x4330)},
     I.ST{sz=I.Word, rs=tmpR, ra=sp, d=I.ImmedOp(cvti2dTmpOff), mem=stack},
     I.L{sz=I.Double, rt=fd, ra=sp, d=I.ImmedOp(cvti2dTmpOff), mem=stack},
     I.L{sz=I.Double, rt=tmpF, ra=sp, d=I.ImmedOp(cvti2dConstOff), mem=stack},
     I.FARITH{oper=I.FSUB, ft=fd, fa=fd, fb=tmpF, Rc=false}]
  end
end