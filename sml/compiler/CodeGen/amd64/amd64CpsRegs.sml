(* amd64CpsRegs.sml
 *
 * COPYRIGHT (c) 2016 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * CPS registers used on the Intel AMD64
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

    fun limitptr vfp 	= regInMem(vfp, 24)
    fun baseptr  vfp	= regInMem(vfp, 8)
    fun exnptr   vfp	= regInMem(vfp, 16)
    fun gcLink   vfp	= regInMem(vfp, 32)
    fun storeptr vfp 	= regInMem(vfp, 48)
    fun varptr   vfp 	= regInMem(vfp, 56)

    fun stdlink  _	= T.REG(64, GP 8) 	
    fun stdclos  _	= T.REG(64, GP 9) 	

    fun mkRegList(n, 0) = []
      | mkRegList(n, cnt) = T.REG(64, GP n)::mkRegList(n+1, cnt-1)

    (* miscregs = {rbx,rcx,rdx,r10,r11,...r15} *)
    val miscregs = rbx::rcx::rdx::mkRegList(10, 6)

    val calleesave  = Array.fromList miscregs
    val exhausted   = NONE

    val floatregs   = map (fn f => T.FREG(64,FP f)) (8 upto 31)
    val savedfpregs = []

    local
      fun unREG (T.REG (_, r)) = r
	| unREG _ = raise Fail "amd64CpsRegs:unREG"
    in

    val availR = map GP (10 upto 15) @ (map unREG [rbp, rsi, rbx, rcx, rdx, rax])
    val dedicatedR = GP 8 :: GP 9 :: (map unREG [rdi, rsp, vfptr])
    val availF = map FP (0 upto 15)
    val dedicatedF = []
    val signedGCTest = false

    val addressWidth = 64

    val ccallCallerSaveR = [unREG rdi]
    val ccallCallerSaveF = []
    end (*local*)

    val wordByteWidth = 8
    val wordBitWidth = 8 * wordByteWidth

  end
