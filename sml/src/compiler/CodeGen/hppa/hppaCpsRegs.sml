(* hppaCpsRegs.sml --- CPS registers used on the HPPA
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

structure HppaCpsRegs : CPSREGS = 
struct
  structure T = HppaMLTree
  structure SL = SortedList

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

  val stdarg	= T.REG(32,GP 11)
  val stdcont	= T.REG(32,GP 12)
  val stdclos	= T.REG(32,GP 10)
  val stdlink	= T.REG(32,GP 9)
  val baseptr	= T.REG(32,GP 8)

  val limitptr	= T.REG(32,GP 4)
  val varptr	= T.REG(32,GP 7)
  val exhausted	= NONE
  val storeptr	= T.REG(32,GP 5)
  val allocptr	= T.REG(32,GP 3)
  val exnptr	= T.REG(32,GP 6)

  val returnPtr	= GP 31
  val gcLink	= T.REG(32,returnPtr)
  val stackptr	= T.REG(32,GP 30)

  val miscregs = 
    map (fn r => T.REG(32,GP r)) 
       [1,13,14,15,16,17,18,19,20,21,22,23,24,25,26,28,2]
  val calleesave = Array.fromList miscregs

  (* Note: We need at least one register for shuffling purposes. *)
  fun fromto(n, m) = if n>m then [] else n :: fromto(n+1, m)
  val floatregs = map (fn f => T.FREG(64,FP f)) (fromto(6, 30))
  val savedfpregs = []

  val allRegs = SL.uniq(fromto(GP 0,GP 31))

  val availR = 
    map (fn T.REG(_,r) => r)
        ([stdlink, stdclos, stdarg, stdcont, gcLink] @ miscregs)
  val dedicatedR = SL.remove(SL.uniq availR, allRegs)

  val availF = SL.uniq(fromto(FP 6, FP 30))

  val allFRegs = SL.uniq(fromto(FP 0,FP 31))

  val dedicatedF = SL.remove(availF, allFRegs)

  val signedGCTest = false
end

(*
 * $Log: hppaCpsRegs.sml,v $
 * Revision 1.3  1998/05/23 14:09:21  george
 *   Fixed RCS keyword syntax
 *
 *)
