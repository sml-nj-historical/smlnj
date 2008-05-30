(* vararg-c-call-fn.sml
 * 
 * Generate MLRISC code that makes a vararg call at runtime. The input to this code is a
 * list of Staged Allocation locations paired with arguments. This code places the arguments
 * in the correct locations.
 *
 * Mike Rainey (mrainey@cs.uchicago.edu)
 *)

functor VarargCCallFn (
    structure T : MLTREE
    structure CCall : C_CALL where T = T
    val gprParams : T.reg list
    val fprParams : T.reg list
    val spReg : T.rexp
    val wordTy : T.ty
    val newReg : 'a -> CellsBasis.cell
  ) = struct

    structure T = T
    structure CB = CellsBasis
    structure CTy = CTypes

    datatype argument = I of int | R of real | B of bool | S of string

    fun argToCTy (I _) = CTy.C_signed CTy.I_int
      | argToCTy (R _) = CTy.C_double
      | argToCTy (B _) = CTy.C_signed CTy.I_int
      | argToCTy (S _) = CTy.C_PTR

    val wordSzB = wordTy div 8
    val mem = T.Region.memory
    val stack = T.Region.stack

    fun lit i = T.LI (T.I.fromInt (wordTy, i))
    fun gpr r = T.GPR (T.REG (wordTy, r))
    fun fpr (ty, f) = T.FPR (T.FREG (ty, f))

    val GPR = 0
    val FPR = 1
    val STK = 2

    val intTy = wordTy

  (* offsets into the triplet *)
    val argOff = 0
    val kindOff = 1
    val locOff = 2

    fun offTrip (arg, off) = T.LOAD(wordTy, T.ADD(wordTy, arg, lit (off*wordSzB)), mem)
    fun offTripF (arg, off) = T.FLOAD(64, T.ADD(wordTy, arg, lit (off*wordSzB)), mem)

    val regToInt = CB.physicalRegisterNum
    fun labelOfReg (k, r) = Label.global ("put"^k^Int.toString (regToInt r))
    val labelOfStk = Label.global "stk"
    val interpLab = Label.global "interp"
    fun chooseRegsLab k = Label.global ("chooseRegs"^k)
    val chooseStkLab = Label.global "chooseStk"
    val chooseKindsLab = Label.global "chooseKinds"

  (* store the argument at the stack offset *)
    fun genStoreStk arg = [
	   T.DEFINE chooseStkLab,
	   T.STORE(wordTy, T.ADD (wordTy, spReg, offTrip(arg, locOff)), offTrip(arg, argOff), mem),
	   T.JMP (T.LABEL interpLab, [])
        ]

  (* place the argument into the parameter register and jump back to the interpreter *)
    fun genPutGpr arg r = [
	   T.DEFINE (labelOfReg ("gpr", r)),
	   T.MV (intTy, r, offTrip (arg, argOff)), 
	   T.JMP (T.LABEL interpLab, [])
        ]

  (* place the argument into the parameter register and jump back to the interpreter *)
    fun genPutFpr arg r = [
	   T.DEFINE (labelOfReg ("fpr", r)),
	   T.FMV (64, r, offTripF (arg, argOff)), 
	   T.JMP (T.LABEL interpLab, [])
        ]

  (* choose the function for loading the register *)
    fun genChooseReg arg k (r, instrs) = let
	   val cmp = T.CMP(wordTy, T.EQ, offTrip(arg, locOff), lit (regToInt r))
           in
	      T.BCC(cmp, labelOfReg (k, r)) :: instrs
	   end

  (* choose registers for loading function arguments *)
    fun genChooseRegs arg k regs = let
	   val instrs = List.rev (List.foldl (genChooseReg arg k) [] regs)
           in
	      T.DEFINE (chooseRegsLab k) :: instrs
	   end

  (* choose the kind of argument *)
    fun genChooseKinds arg = [
	   T.DEFINE chooseKindsLab,
	   T.BCC(T.CMP(wordTy, T.EQ, offTrip(arg, kindOff), lit GPR), chooseRegsLab "gpr"),
	   T.BCC(T.CMP(wordTy, T.EQ, offTrip(arg, kindOff), lit FPR), chooseRegsLab "fpr"),
	   T.BCC(T.CMP(wordTy, T.EQ, offTrip(arg, kindOff), lit STK), chooseStkLab)
        ]

    val NIL = 0

    fun offArgs args 0 = T.LOAD (wordTy, T.REG(wordTy, args), mem)
      | offArgs args off = T.LOAD (wordTy, T.ADD (wordTy, T.REG(wordTy, args), lit(off*8)), mem)

    val gotoCLab = Label.label "gotoC" ()

  (* call the varargs C function *)
    fun genCallC cFun = let
	   val defs = List.map gpr CCall.callerSaveRegs @ List.map (fn r => fpr(64, r)) CCall.callerSaveFRegs
	   val uses = List.map gpr gprParams @ List.map (fn r => fpr(64, r)) fprParams
	   in
	      [
	       T.DEFINE gotoCLab,
	       T.CALL {funct=cFun, targets=[], defs=defs, uses=uses, region=mem, pops=0}
	      ]
	   end

  (* interpreter for varargs *)
    fun genInterp (args, argReg) = [
	   T.DEFINE interpLab,
	 (* loop through the args *)
	   T.BCC (T.CMP(wordTy, T.EQ, T.REG (wordTy, args), lit NIL), gotoCLab),
	   T.MV (wordTy, argReg, offArgs args 0),
	   T.MV(wordTy, args, offArgs args 1),
	   T.JMP (T.LABEL chooseKindsLab, [])		
        ]

  (* generate instructions for making a varargs call *)
    fun genVarargs (cFun, args) = let
	   val argReg = newReg ()
	   val interpInstrs = genInterp(args, argReg)
	   val arg = T.REG(wordTy, argReg)
	   val ckInstrs = genChooseKinds arg
	   val chooseGprs = genChooseRegs arg "gpr" gprParams
	   val chooseFprs = genChooseRegs arg "fpr" fprParams
	   val loadGprs = List.concat (List.map (genPutGpr arg) gprParams)
	   val loadFprs = List.concat (List.map (genPutFpr arg) fprParams)
	   val storeStk = genStoreStk arg
           in
	      List.concat [
	         interpInstrs,
		 ckInstrs,
		 chooseGprs,
		 chooseFprs,
		 loadGprs,
		 loadFprs,
		 storeStk,
		 genCallC cFun
	      ]
	   end


  end (* AMD64VarargCCallFn *)
