(* alpha32CpsRegs.sml --- CPS registers used on the DEC Alpha
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

structure Alpha32CpsRegs : CPSREGS = 
struct
  structure T = Alpha32MLTree
  structure SL = SortedList
  fun upto (from,to) = if from>to then [] else from::(upto (from+1,to))
  infix upto

  val stdarg	= T.REG 0
  val stdcont  	= T.REG 1
  val stdclos	= T.REG 2
  val stdlink	= T.REG 3
  val baseptr   = T.REG 4
  val maskreg	= T.REG 5

  val limitptr 	= T.REG 9
  val varptr	= T.REG 10
  val exhaustedR = 11
  val exhausted	= T.CC  exhaustedR
  val storeptr	= T.REG 12
  val allocptr 	= T.REG 13
  val exnptr	= T.REG 14

  val gclinkreg	= T.REG 26
  val stackptr	= T.REG 30

  val miscregs =  map T.REG ((6 upto 8) @ (16 upto 25) @ [27])
  val calleesave = Array.fromList(miscregs)
  val floatregs = map T.FREG (0 upto 28)
  val savedfpregs = []

  val allRegs = SL.uniq(0 upto 31)

  val availR = 
    map (fn T.REG r => r)
         ([gclinkreg, maskreg, T.REG exhaustedR, 
	   stdlink, stdclos, stdarg, stdcont] @ miscregs)
  val dedicatedR = SL.remove(SL.uniq availR, allRegs)

  val availF = 0 upto 28
  val dedicatedF = [29, 30, 31]
end

(*
 * $Log: alpha32CpsRegs.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:54  george
 * Version 110.5
 *
 *)
