(* hppaCpsRegs.sml --- CPS registers used on the HPPA
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

structure HppaCpsRegs : CPSREGS = 
struct
  structure T = HppaMLTree

  (* HPPA register conventions 
     0     zero
     1	   caller-saves
     2     return-pointer and scratch
     3-18  callee-saves
     19-22 caller-saves
     23    arg3
     24    arg2
     25    arg1
     26    arg0
     27    reserved
     28    ret0
     29    ret1
     30    stack pointer
     31    millicode return and scratch.
   *)

  val GP = HppaCells.GPReg
  val FP = HppaCells.FPReg
  fun REG r = T.REG(32, GP r) 
  fun FREG f = T.FREG(64, FP f)

  val stdarg	= REG(11)
  val stdcont	= REG(12)
  val stdclos	= REG(10)
  val stdlink	= REG(9)
  val baseptr	= REG(8)

  val limitptr	= REG(4)
  val varptr	= REG(7)
  val exhausted	= NONE
  val storeptr	= REG(5)
  val allocptr	= REG(3)
  val exnptr	= REG(6)

  val returnPtr	= GP 31
  val gcLink	= T.REG(32,returnPtr) 
  val stackptr	= REG(30)

  val miscregs = map REG [1,13,14,15,16,17,18,19,20,21,22,23,24,25,26,28,2]
  val calleesave = Array.fromList miscregs 

  (* Note: We need at least one register for shuffling purposes. *)
  fun fromto(n, m) = if n>m then [] else n :: fromto(n+1, m)
  val floatregs = map FREG (fromto(6, 30))
  val savedfpregs = []

  val availR = 
    map (fn T.REG(_,r) => r)
        ([stdlink, stdclos, stdarg, stdcont, gcLink] @ miscregs)

  local
      structure SC = HppaCells.SortedCells
      val -- = SC.difference
      infix --
  in
      val allRegs = map GP (fromto(0,31))
      val dedicatedR = SC.return (SC.uniq allRegs -- SC.uniq availR)

      val availFs = map FP (fromto(6, 30))
      val allFRegs = map FP (fromto(0, 31))
      val dedicatedF = SC.return (SC.uniq allFRegs -- SC.uniq availFs)
      val availF = availFs
  end

  val signedGCTest = false
  val addressWidth = 32
end

