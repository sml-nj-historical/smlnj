(* X86CpsRegs.sml --- CPS registers used on the Intel X86
 *
 * COPYRIGHT (c) 1997 Bell Laboratories.
 *
 *)
signature X86CPSREGS = sig
  include CPSREGS
end

structure X86CpsRegs : CPSREGS = struct
  structure T = X86MLTree
  structure C = X86Cells

  fun upto(from, to) = if from>to then [] else from::(upto (from+1,to))
  infix upto 

  val GP = C.GPReg
  val FP = C.FPReg

  val eax = T.REG(32, C.eax)	val esp = T.REG(32, C.esp)
  val ecx = T.REG(32, C.ecx)	val ebp = T.REG(32, C.ebp)
  val edx = T.REG(32, C.edx)	val esi = T.REG(32, C.esi)
  val ebx = T.REG(32, C.ebx)	val edi = T.REG(32, C.edi)

  fun regInMem i = T.LOAD(32, T.ADD(32, esp, T.LI i), CPSRegions.memory)

  val allocptr 	= edi
  val stdarg 	= ebp
  val stdcont   = esi
  val stackptr 	= esp

  val baseptr	= regInMem 4
  val exnptr 	= regInMem 8
  val limitptr 	= regInMem 12
  val gcLink	= regInMem 16
  val storeptr 	= regInMem 24
  val varptr  	= regInMem 28

  val stdlink	= T.REG(32, GP 8)		(* vreg 0 *)
  val stdclos	= T.REG(32, GP 9)		(* vreg 1 *)

  fun mkVregList(n, 0) = []
    | mkVregList(n, cnt) = T.REG(32, GP n)::mkVregList(n+1, cnt-1)

  (* miscregs = {ebx,ecx,edx,r10,r11,...r31} *)
  val miscregs  = ebx::ecx::edx::mkVregList(10, X86Runtime.numVregs - 2)

  val calleesave = Array.fromList miscregs
  val exhausted = NONE

  val floatregs = map (fn f => T.FREG(64,f)) ((FP 8) upto (FP 31))
  val savedfpregs = []

  val availR = map (fn T.REG(_,r) => r) [ebp, esi, ebx, ecx, edx, eax]
  val dedicatedR = map (fn T.REG(_,r) => r) [edi, esp]
  val availF = (FP 8) upto (FP 31)
  val dedicatedF = map FP [0,1,2,3,4,5,6,7]
  val signedGCTest = false
end


