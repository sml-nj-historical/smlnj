structure VarargCall =
  struct

    val regNum = CellsBasis.physicalRegisterNum

    structure X86SA = StagedAllocationFn (
                         type reg_id = int
			 datatype loc_kind = datatype CLocKind.loc_kind
			 val memSize = 8)
    structure X86Convention = X86CConventionFn(
			        type reg_id = int
				val eax = regNum X86Cells.eax
				val edx = regNum X86Cells.edx
				val st0 = regNum X86Cells.ST0
				structure SA = X86SA)
    structure X86VarargCall = VarargCallFn (
			          val params = X86Convention.params
				  val returns = X86Convention.returns
				  val store0 = X86Convention.store0
				  val bitWidthOfPointer = 32
				  val alignBOfPointer = 4
				  val alignBOfInt = 4
				  val alignBOfDouble = 4
				  val kindOfInt = CLocKind.GPR
				  val kindOfPointer = CLocKind.GPR
				  val kindOfDouble = CLocKind.FPR
				  structure SA = X86SA
			      )

    structure X86_64SA = StagedAllocationFn (
                         type reg_id = int
			 datatype loc_kind = datatype CLocKind.loc_kind
			 val memSize = 8)
    structure X86_64Convention = X86_64CConventionFn(
			        type reg_id = int
				val rax = regNum AMD64Cells.rax
				val rdi = regNum AMD64Cells.rdi
				val rsi = regNum AMD64Cells.rsi
				val rdx = regNum AMD64Cells.rdx
				val rcx = regNum AMD64Cells.rcx
				val r8 = regNum AMD64Cells.r8
				val r9 = regNum AMD64Cells.r9
				val xmm0 = regNum AMD64Cells.xmm0
				val xmm1 = regNum AMD64Cells.xmm1
				val xmm2 = regNum AMD64Cells.xmm2
				val xmm3 = regNum AMD64Cells.xmm3
				val xmm4 = regNum AMD64Cells.xmm4
				val xmm5 = regNum AMD64Cells.xmm5
				val xmm6 = regNum AMD64Cells.xmm6
				val xmm7 = regNum AMD64Cells.xmm7
				structure SA = X86_64SA)
    structure X86_64VarargCall = VarargCallFn (
			          val params = X86_64Convention.params
				  val returns = X86_64Convention.returns
				  val store0 = X86_64Convention.store0
				  val bitWidthOfPointer = 64
				  val alignBOfPointer = 8
				  val alignBOfInt = 8
				  val alignBOfDouble = 8
				  val kindOfInt = CLocKind.GPR
				  val kindOfPointer = CLocKind.GPR
				  val kindOfDouble = CLocKind.FPR
				  structure SA = X86_64SA
			      )

    local
	structure V = Vararg
	structure DL = DynLinkage
	fun main's s = DL.lib_symbol (DL.main_lib, s)
	val printf_h = main's "printf"
	fun call args = X86VarargCall.dispatchLowlevelCall(printf_h, args)
    in
    fun ex1 () = call [V.STRING_ARG "test123\n"]
    fun ex2 () = call [V.STRING_ARG "%d %s\n", V.SINT_ARG 1024, V.STRING_ARG "xxx"]
    fun ex3 () = call [V.STRING_ARG "%d %s %d\n", V.SINT_ARG 1024, V.STRING_ARG "xxx", V.SINT_ARG 222333]
    end

  end
