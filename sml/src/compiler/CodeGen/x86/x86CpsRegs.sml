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

  fun upto(from, to) = if from>to then [] else from::(upto (from+1,to))
  infix upto 

  val eax = T.REG 0	val esp = T.REG 4
  val ecx = T.REG 1	val ebp = T.REG 5
  val edx = T.REG 2	val esi = T.REG 6
  val ebx = T.REG 3	val edi = T.REG 7

  fun regInMem i = T.LOAD32(T.ADD(esp, T.LI i), CPSRegions.memory)

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

  val stdlink	= T.REG 8		(* vreg 0 *)
  val stdclos	= T.REG 9		(* vreg 1 *)

  fun mkVregList(n, 0) = []
    | mkVregList(n, cnt) = T.REG n::mkVregList(n+1, cnt-1)

  (* miscregs = {ebx,ecx,edx,r10,r11,...r31} *)
  val miscregs  = ebx::ecx::edx::mkVregList(10, X86Runtime.numVregs - 2)

  val calleesave = Array.fromList miscregs
  val exhausted = NONE

  val floatregs = map T.FREG (8 upto 31)
  val savedfpregs = []

  val availR = map (fn T.REG r => r) [ebp, esi, ebx, ecx, edx, eax]
  val dedicatedR = map (fn T.REG r => r) [edi, esp]
  val availF = 8 upto 31
  val dedicatedF = [0,1,2,3,4,5,6,7]
end


