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
  (* registers for passing parameters *)
    val gprParams : T.reg list
    val fprParams : T.reg list
  (* possible widths for passing parameters *)
    val gprTys : T.ty list
    val fprTys : T.ty list
  (* stack pointer register *)
    val spReg : T.rexp
  (* default register width *)
    val wordTy : T.ty
    val newReg : 'a -> CellsBasis.cell
  )  = struct

    structure T = T
    structure CB = CellsBasis
    structure Consts = VarargCCallConstants

    fun concatMap f xs = List.concat (List.map f xs)

    val mem = T.Region.memory
    val stack = T.Region.stack
    val defaultWidthB = Consts.defaultWidthB

    fun lit i = T.LI (T.I.fromInt (wordTy, i))
    fun gpr r = T.GPR (T.REG (wordTy, r))
    fun fpr (ty, f) = T.FPR (T.FREG (ty, f))
    val regToInt = CB.physicalRegisterNum

  (* load a value from the zipped argument *)
    fun offZippedArg (ty, arg, off) = T.LOAD(ty, T.ADD(wordTy, arg, lit (off*defaultWidthB)), mem)

  (* load a floating-point value from the zipped argument *)
    fun offZippedArgF (ty, arg, off) = T.FLOAD(ty, T.ADD(wordTy, arg, lit (off*defaultWidthB)), mem)

    fun newLabel s = Label.label s ()

    fun labelOfReg (k, ty, r) = 
	    Label.global (k^Int.toString ty^"."^Int.toString (regToInt r)^".reg")

    local
      fun atTyLab (k, tys) = List.map (fn ty => (ty, newLabel (k^Int.toString ty^"."))) tys
      val gprLabs = atTyLab ("resolveGprs", gprTys)
      val fprLabs = atTyLab ("resolveFprs", fprTys)
      val stkLabs = atTyLab ("resolveStk", gprTys)
      val fstkLabs = atTyLab ("resolveFstk", fprTys)
    in
    fun resolveAtKindAndTyLab (k, ty) = let
	   val labs = (case k
			of "gpr" => gprLabs
			 | "fpr" => fprLabs
			 | "stk" => stkLabs
			 | "fstk" => fstkLabs
		      (* end case *))
	   val SOME (_, lab) = List.find (fn (ty', _) => ty = ty') labs
           in
	      lab
           end
    end

    local
      fun atKindLab k = (k, newLabel ("resolveTys."^k))
      val labs = List.map atKindLab ["gpr", "fpr", "stk", "fstk"]
    in
    fun resolveTysLab k = let
	    val SOME (_, lab) = List.find (fn (k', _) => k' = k) labs
            in
	        lab
	    end
    end

    val interpLab = newLabel "interp"
    val resolveKindsLab = newLabel "resolveKinds"
    val gotoCLab = newLabel "gotoC"

  (* store a gpr argument on the stack *)
    fun storeStk (arg, ty) = 
	    T.STORE(ty, T.ADD (wordTy, spReg, offZippedArg(wordTy, arg, Consts.locOff)), offZippedArg(ty, arg, Consts.argOff), mem)

    fun genStoreStkAtTy arg ty = [
	   T.DEFINE (resolveAtKindAndTyLab ("stk", ty)),
	   storeStk(arg, ty),
	   T.JMP (T.LABEL interpLab, [])
        ]

  (* store the argument at the stack offset *)
    fun genStoreStk arg tys = concatMap (genStoreStkAtTy arg) tys

  (* store a fpr argument on the stack *)
    fun storeFStk (arg, ty) =
	    T.FSTORE(ty, T.ADD (wordTy, spReg, offZippedArg(wordTy, arg, Consts.locOff)), offZippedArgF(ty, arg, Consts.argOff), mem)

  (* store the argument at the stack offset *)
    fun genStoreFStkAtTy arg ty = [
	   T.DEFINE (resolveAtKindAndTyLab ("fstk", ty)),
	   storeFStk (arg, ty),
	   T.JMP (T.LABEL interpLab, [])
        ]

  (* store the argument at the stack offset *)
    fun genStoreFStk arg tys = concatMap (genStoreFStkAtTy arg) tys

  (* place the argument into the parameter register and jump back to the interpreter *)
    fun genPutGpr arg ty r = [
	   T.DEFINE (labelOfReg ("gpr", ty, r)),
	   T.MV (ty, r, offZippedArg (ty, arg, Consts.argOff)), 
	   T.JMP (T.LABEL interpLab, [])
        ]

  (* place the argument into the parameter register and jump back to the interpreter *)
    fun genPutFpr arg ty r = [
	   T.DEFINE (labelOfReg ("fpr", ty, r)),
	   T.FMV (ty, r, offZippedArgF (ty, arg, Consts.argOff)),
	   T.JMP (T.LABEL interpLab, [])
        ]

  (* resolve the function for loading the register *)
    fun genResolveReg arg k ty (r, instrs) = let
	   val cmp = T.CMP(wordTy, T.EQ, offZippedArg(wordTy, arg, Consts.locOff), lit (regToInt r))
           in
	      T.BCC(cmp, labelOfReg (k, ty, r)) :: instrs
	   end

  (* check the type of the argument *)
    fun checkTy arg k ty = 
	    T.BCC(T.CMP(wordTy, T.EQ, offZippedArg(wordTy, arg, Consts.tyOff), lit ty), resolveAtKindAndTyLab(k, ty))

  (* resolve the type of the argument at a given kind *)
    fun genResolveTysAtKind arg (k, tys) = 
	 T.DEFINE (resolveTysLab k) :: List.map (checkTy arg k) tys

  (* resolve the type of the argument *)
    fun genResolveTys arg = 
	    concatMap (genResolveTysAtKind arg) [("gpr", gprTys), ("fpr", fprTys), ("stk", gprTys), ("fstk", fprTys)]

  (* resolve registers at a fixed type *)
    fun genResolveRegsOfTy arg k ty regs = 
	    T.DEFINE (resolveAtKindAndTyLab (k, ty)) :: 
	    List.rev (T.JMP(T.LABEL interpLab, []) :: List.foldl (genResolveReg arg k ty) [] regs)

  (* resolve registers for loading function arguments *)
    fun genResolveRegs arg k tys regs = let
	    val resolves = List.map (genResolveRegsOfTy arg k) tys
            in
	       concatMap (fn f => f regs) resolves
	    end

  (* resolve an argument to a kind of location *)
    fun resolveKind arg (kEncoding, k) = 
	    T.BCC(T.CMP(wordTy, T.EQ, offZippedArg(wordTy, arg, Consts.kindOff), lit kEncoding), resolveTysLab k)

  (* resolve the argument to one of the location kinds *)
    fun genResolveKinds arg = 
	   T.DEFINE resolveKindsLab ::
	   List.map (resolveKind arg) [(Consts.GPR, "gpr"), (Consts.FPR, "fpr"), (Consts.STK, "stk"), (Consts.FSTK, "fstk")]

    fun resolveArgLocs arg = List.concat [
           genResolveRegs arg "gpr" gprTys gprParams,
	   genResolveRegs arg "fpr" fprTys fprParams,
	   genStoreStk arg gprTys,
	   genStoreFStk arg fprTys
        ]

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
    fun genInterp (args, argsReg, endOfArgs) = [
            T.DEFINE interpLab,
	  (* loop through the args *)
	    T.MV (wordTy, argsReg, T.ADD (wordTy, args, lit Consts.zippedArgSzB)),
	    T.BCC (T.CMP(wordTy, T.GE, args, endOfArgs), gotoCLab)
          ]

  (* generate all possible general-purpose register loads *)
    fun genPutGprs arg = let
	    val putGprs = List.map (genPutGpr arg) gprTys
            in
	       concatMap (fn f => concatMap f gprParams) putGprs
	    end

  (* generate all possible floating-point register loads *)
    fun genPutFprs arg = let
	    val putfprs = List.map (genPutFpr arg) fprTys
            in
	       concatMap (fn f => concatMap f fprParams) putfprs
	    end

  (* generate instructions for making a varargs call *)
    fun genVarargs (cFun, argsReg, endOfArgs) = let           
	    val arg = T.REG (wordTy, argsReg)
            in
	      List.concat [
	         genInterp(arg, argsReg, endOfArgs),
		 genResolveKinds arg,
		 resolveArgLocs arg,
		 genResolveTys arg,
		 genPutGprs arg,
		 genPutFprs arg,
		 genCallC cFun
	      ]
	    end

  end (* VarargCCallFn *)
