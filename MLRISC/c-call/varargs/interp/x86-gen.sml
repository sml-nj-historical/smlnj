(* x86-gen.sml
 *
 * X86-specific portion of the interpreter.
 *)

structure X86Gen =
  struct

    val defTy = 32

    structure T = X86MLTree
    structure C = X86Cells
    structure CB = CellsBasis
    structure Consts = VarargConstants

    fun gpr r = T.GPR(T.REG(32, r))
    val calleeSaveRegs = List.map gpr [C.ebx, C.esi, C.edi]
    fun push e = T.EXT(X86InstrExt.PUSHL e)
    val leave = T.EXT X86InstrExt.LEAVE

    structure Gen = GenFn (
	structure T = T
	val gprs = []
	val fprs = []
	val gprWidths = [8, 16, 32]
	val fprWidths = [32, 64]
	val spReg = T.REG(32, C.esp)
	val defaultWidth = 32
	val callerSaves = [C.eax, C.ecx, C.edx]
	val callerSavesF = [])

    fun lit i = T.LI (T.I.fromInt (defTy, i))

  (* get the ith argument in the calling sequence *)
    fun getArg i = 
	    T.LOAD(defTy, T.ADD(defTy, T.REG(defTy, C.ebp), lit (4*i+8)), T.Region.memory)

  (* MLRISC code for the x86 vararg interpreter *)
    fun gen () = let
	   val largsReg = C.newReg()
         (* we align the frame to a 16-bytes to support Mac OS. *)
	   val frameSzB = 1024*4-2*4
	   val interpFunPtr = getArg 0
	   val endOfLargs = getArg 2
           in
	       List.concat [
	         (* preserve callee-save registers *)
	           [T.LIVE calleeSaveRegs],
		   [push (T.REG(defTy, C.ebp))],
		   [T.COPY (defTy, [C.ebp], [C.esp])],
		   [T.MV(defTy, largsReg, getArg 1)],
		 (* allocate stack space for the arguments *)
		   [T.MV(defTy, C.esp, T.SUB(defTy, T.REG(defTy, C.esp), lit frameSzB))],
	           Gen.gen {interpFunPtr=interpFunPtr, largsReg=largsReg, endOfLargs=endOfLargs},
		   [leave],
	           [T.LIVE calleeSaveRegs],
		   [T.RET []]
		   ]
	   end

    fun main (cmd, args) = let
	   val _ = Label.reset()
	   val lab = Label.global Consts.varargInterpreter
	   val stms = gen()
	   val asmOutStrm = TextIO.openOut "vararg-interp-x86-linux.s"
	   val _ = TextIO.output(asmOutStrm, Consts.header^"\n")
	   fun doit () = X86MLRISCGen.dumpOutput(X86MLRISCGen.codegen'(lab, stms, [T.GPR (T.REG (defTy, C.eax))]))
	   val _ = AsmStream.withStream asmOutStrm doit ()
	   val _ = TextIO.closeOut asmOutStrm
           in
	      0
	   end

  end
