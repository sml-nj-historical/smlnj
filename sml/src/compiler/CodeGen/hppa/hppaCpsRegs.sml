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

  val stdarg	= T.REG 11
  val stdcont	= T.REG 12
  val stdclos	= T.REG 10
  val stdlink	= T.REG 9
  val baseptr	= T.REG 8
  val maskreg	= T.REG 20

  val limitptr	= T.REG 4
  val varptr	= T.REG 7
  val exhausted	= NONE
  val storeptr	= T.REG 5
  val allocptr	= T.REG 3
  val exnptr	= T.REG 6

  val returnPtr	= 31
  val gclinkreg	= T.REG returnPtr
  val stackptr	= T.REG 30

  val miscregs = 
    map T.REG [1, 13, 14, 15, 16, 17, 18, 19, 22, 23, 24, 25, 26, 28, 2, 21]
  val calleesave = Array.fromList miscregs

  (* Note: We need at least one register for shuffling purposes. *)
  fun fromto(n, m) = if n>m then [] else n :: fromto(n+1, m)
  val floatregs = map T.FREG (fromto(6, 30))
  val savedfpregs = []

  val allRegs = SL.uniq(fromto(0,31))

  val availR = 
    map (fn T.REG r => r)
        ([stdlink, stdclos, stdarg, stdcont, gclinkreg, maskreg] @ miscregs)
  val dedicatedR = SL.remove(SL.uniq availR, allRegs)

  val availF = SL.uniq(fromto(6, 30))
  val dedicatedF = SL.remove(availF, allRegs)
end

(*
 * $Log: hppaCpsRegs.sml,v $
 * Revision 1.2  1998/05/08 10:52:23  george
 *   The exhausted register has been made optional -- leung
 *
 * Revision 1.1.1.1  1998/04/08 18:39:55  george
 * Version 110.5
 *
 *)
