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

  val vfp = C.newDedicatedCell C.GP ()
  val vfptr = T.REG(32, vfp)
  
  fun regInMem(which, i) = let
    val fp = if which then vfptr else esp
  in T.LOAD(32, T.ADD(32, fp, T.LI(T.I.fromInt(32, i))), CPSRegions.memory) 
  end

  val allocptr 	    = edi
  val stackptr      = esp
  fun stdarg _      = ebp
  fun stdcont _     = esi


  fun limitptr vfp 	= regInMem(vfp, 12)
  fun baseptr  vfp	= regInMem(vfp, 4)
  fun exnptr   vfp	= regInMem(vfp, 8)
  fun gcLink   vfp	= regInMem(vfp, 16)
  fun storeptr vfp 	= regInMem(vfp, 24)
  fun varptr   vfp 	= regInMem(vfp, 28)

  fun stdlink  _	= T.REG(32, GP 8) 	(* vreg 0 *)
  fun stdclos  _	= T.REG(32, GP 9) 	(* vreg 1 *)

  fun mkVregList(n, 0) = []
    | mkVregList(n, cnt) = T.REG(32, GP n)::mkVregList(n+1, cnt-1)

  (* miscregs = {ebx,ecx,edx,r10,r11,...r31} *)
  val miscregs =
      ebx::ecx::edx::mkVregList(10, X86Runtime.numVregs - 2)

  val calleesave  = Array.fromList miscregs
  val exhausted   = NONE

  val floatregs   = map (fn f => T.FREG(64,FP f)) (8 upto 31)
  val savedfpregs = []

  val availR = map (fn T.REG(_,r) => r) [ebp, esi, ebx, ecx, edx, eax]
  val dedicatedR = map (fn T.REG(_,r) => r) [edi, esp, vfptr]
  val availF = map FP (8 upto 31)
  val dedicatedF = [] (* map FP [0,1,2,3,4,5,6,7] *)
  val signedGCTest = false
  val addressWidth = 32
end


