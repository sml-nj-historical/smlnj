(* sparcCpsRegs.sml --- CPS registerS USED ON the Sparc
 *
 * COPYRIGHT (c) 1998 AT&T Bell Laboratories.
 *
 *)

structure SparcCpsRegs : CPSREGS = 
struct
  structure T = SparcMLTree
  structure SL = SortedList

  val stdarg	= T.REG 24  (* %i0 *)
  val stdcont	= T.REG 25  (* %i1 *)
  val stdclos	= T.REG 26  (* %i2 *)
  val stdlink	= T.REG 1   (* %g1 *)
  val baseptr	= T.REG 27  (* %i3 *)
  val maskreg	= T.REG 13  (* %o5 *)

  val limitptr	= T.REG 4   (* %g4 *)
  val varptr	= T.REG 29  (* %i5 *)
  val exhausted	= SOME(T.CC 65)   (* %psr *) 
  val storeptr	= T.REG 5   (* %g5 *)
  val allocptr	= T.REG 6   (* %g6 *)
  val exnptr	= T.REG 7   (* %g7 *)

  val returnPtr	= 15        
  val gclinkreg	= T.REG returnPtr
  val stackptr	= T.REG 14  

   (* Warning %o2 is used as the asmTmp
    *)    
  val miscregs = (* %g2-%g3, %o0-%o1, %l0-%l7, %i4, %o3-%o4 *)
    map T.REG [2, 3, 8, 9, 16, 17, 18, 19, 20, 21, 22, 23, 28, 11, 12]
  val calleesave = Array.fromList miscregs

  (* Note: We need at least one register for shuffling purposes. *)
  fun fromto(n, m, inc) = if n>m then [] else n :: fromto(n+inc, m, inc)
  val floatregs = map T.FREG (fromto(0,31,2))
  val savedfpregs = []

  val allRegs = SL.uniq(fromto(0,31,1))

  val availR = 
    map (fn T.REG r => r)
        ([stdlink, stdclos, stdarg, stdcont, gclinkreg, maskreg] @ miscregs)
  val dedicatedR = SL.remove(SL.uniq availR, allRegs)

  val availF = SL.uniq(fromto(0, 30, 2))
  val dedicatedF = []
end

(*
 * $Log: sparcCpsRegs.sml,v $
 * Revision 1.1.1.1  1998/08/05 19:37:50  george
 *   Release 110.7.4
 *
 *)
