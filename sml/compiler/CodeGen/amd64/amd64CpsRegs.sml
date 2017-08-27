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

    fun upto(from, to) = if from>to then [] else from::(upto (from+1,to))
    infix upto 

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

    val stackptr	= rsp

    val allocptr	= rdi
    fun limitptr _ 	= r14
    fun storeptr _ 	= r15
    fun stdarg _	= rbp
    fun stdcont _	= rsi
    fun stdlink _	= r08
    fun stdclos _	= r09

  (* offsets are w.r.t. the stack pointer.  See
   *
   *	https://smlnj-gforge.cs.uchicago.edu/svn/smlnj/dev-notes/amd64-stack-frame.numbers
   *
   * for details.
   *)
    fun baseptr  vfp	= regInMem(vfp, 32)
    fun exnptr   vfp	= regInMem(vfp, 40)
    fun gcLink   vfp	= regInMem(vfp, 48)
    fun varptr   vfp 	= regInMem(vfp, 56)

    val miscregs = [rbx, rcx, rdx, r10, r11, r12, r13]

    val calleesave  = Array.fromList miscregs
    val exhausted   = NONE

    val floatregs   = map (fn f => T.FREG(64, FP f)) (0 upto 15)
    val savedfpregs = []

    local
      fun unREG (T.REG (_, r)) = r
	| unREG _ = raise Fail "amd64CpsRegs:unREG"
    in

    val availR = List.map unREG (rax :: stdlink false :: stdclos false
	  :: stdarg false :: stdcont false :: miscregs)
    val dedicatedR = List.map unREG [
	    stackptr, allocptr, limitptr false, storeptr false
	  ]
    val availF = map FP (0 upto 15)
    val dedicatedF = []
    val signedGCTest = false

  (* NOTE: these lists reflect the System V ABI, but Windows has a different convention! *)
    val ccallCallerSaveR = [
	    C.rax, C.rcx, C.rdx, C.rsi, C.rdi, C.r8, C.r9, C.r10, C.r11
	  ]
    val ccallCallerSaveF = List.tabulate(16, FP)
    end (*local*)

  end
