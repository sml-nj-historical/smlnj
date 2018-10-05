(* amd64CpsRegs.sml
 *
 * COPYRIGHT (c) 2016 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * CPS registers used on the AMD64
 *)

signature AMD64CPSREGS = sig
    include CPSREGS
  end

structure AMD64CpsRegs : CPSREGS =
  struct
    structure T = AMD64MLTree
    structure C = AMD64Cells

    val GP = C.GPReg
    val FP = C.FPReg

    val rax = T.REG(64, C.rax)	val rsp = T.REG(64, C.rsp)
    val rcx = T.REG(64, C.rcx)	val rbp = T.REG(64, C.rbp)
    val rdx = T.REG(64, C.rdx)	val rsi = T.REG(64, C.rsi)
    val rbx = T.REG(64, C.rbx)	val rdi = T.REG(64, C.rdi)
    val r08 = T.REG(64, C.r8)   val r09 = T.REG(64, C.r9)
    val r10 = T.REG(64, C.r10)  val r11 = T.REG(64, C.r11)
    val r12 = T.REG(64, C.r12)  val r13 = T.REG(64, C.r13)
    val r14 = T.REG(64, C.r14)  val r15 = T.REG(64, C.r15)

    val vfp = C.newDedicatedCell CellsBasis.GP ()
    val vfptr = T.REG(64, vfp)

    fun frameptr which = if which then vfptr else rsp

    fun regInMem (which, i) = let
          val fp = frameptr which
          in
	    T.LOAD(64, T.ADD(64, fp, T.LI(T.I.fromInt(32, i))), CPSRegions.memory)
          end

    val allocptr	= rdi
    val stackptr	= rsp
    fun stdarg _	= rbp
    fun stdcont _	= rsi
    fun stdlink  _	= r08
    fun stdclos  _	= r09
    fun limitptr _ 	= r10
    fun storeptr vfp 	= r11
    fun baseptr  vfp	= regInMem(vfp, 8)
    fun exnptr   vfp	= regInMem(vfp, 16)
    fun gcLink   vfp	= regInMem(vfp, 32)
    fun varptr   vfp 	= regInMem(vfp, 56)

    fun mkRegList (base, cnt) = List.tabulate(cnt, fn i => T.REG(64, GP(base+i)))

  (* miscregs = {rbx,rcx,rdx,r10,r11,...r15} *)
    val miscregs = rbx::rcx::rdx::mkRegList(10, 6)

    val calleesave  = Array.fromList miscregs
    val exhausted   = NONE

    val floatregs   = List.tabulate(16, fn f => T.FREG(64,FP f))
    val savedfpregs = []

    local
      fun unREG (T.REG (_, r)) = r
	| unREG _ = raise Fail "amd64CpsRegs:unREG"
    in

    val availR = List.tabulate(6, fn i => GP(10+i)) @ (map unREG [rbp, rsi, rbx, rcx, rdx, rax])
    val dedicatedR = GP 8 :: GP 9 :: (map unREG [rdi, rsp, vfptr])
    val availF = List.tabulate(16, FP)
    val dedicatedF = []
    val signedGCTest = false

    val ccallCallerSaveR = [C.rax, C.rdi]
    val ccallCallerSaveF = []
    end (*local*)

  end
