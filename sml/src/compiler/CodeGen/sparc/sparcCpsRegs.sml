(* sparcCpsRegs.sml --- CPS registerS USED ON the Sparc
 *
 * COPYRIGHT (c) 1998 AT&T Bell Laboratories.
 *
 *)

structure SparcCpsRegs : CPSREGS = 
struct
  structure T = SparcMLTree
  structure C = SparcCells

  val GP = C.GPReg
  val FP = C.FPReg

  fun REG r = T.REG(32,GP r) 
  fun FREG f = T.FREG(64,FP f)

  val vfp		= SparcCells.newReg()
  val vfptr		= T.REG(32, vfp)
  
  fun stdarg _		= REG(24) (* %i0 *)
  fun stdcont _		= REG(25) (* %i1 *)
  fun stdclos _		= REG(26) (* %i2 *)
  fun stdlink _		= REG(1)  (* %g1 *)
  fun baseptr _		= REG(27) (* %i3 *)

  fun limitptr _	= REG(4)  (* %g4 *)
  fun varptr _		= REG(29) (* %i5 *)
  val exhausted		= SOME(T.CC(T.GTU,C.psr))  (* %psr *) 
  fun storeptr _	= REG(5)  (* %g5 *)
  val allocptr		= REG(6)  (* %g6 *)
  fun exnptr _		= REG(7)  (* %g7 *)

  val returnPtr		= GP 15        
  fun gcLink _		= T.REG(32,returnPtr) 
  val stackptr		= REG(14)

   (* Warning %o2 is used as the asmTmp
    *)    
  val miscregs =
    map REG
              [2, 3,				(* %g2-%g3 *)
	       8, 9,				(* %o0-%o1 *)
	       16, 17, 18, 19, 20, 21, 22, 23,  (* %l0-%l7 *)
	       28, 31,				(* %i4, %i6, %i7 *)
	       11, 12, 13]			(* %o3-%o5 *)
  val calleesave = Array.fromList miscregs

  (* Note: We need at least one register for shuffling purposes. *)
  fun fromto(n, m, inc) = if n>m then [] else n :: fromto(n+inc, m, inc)
  val floatregs = map FREG (fromto(0,31,2))
  val savedfpregs = []

  val availR = 
    map (fn T.REG(_,r) => r)
        ([stdlink(false), stdclos(false), stdarg(false), stdcont(false), gcLink(false)] @ miscregs)

  local
      structure SC = SparcCells.SortedCells
      val -- = SC.difference
      infix --
  in
      val allRegs = map GP (fromto(0, 31, 1))
      val dedicatedR = SC.return (SC.uniq allRegs -- SC.uniq availR)

      val availF =  map FP (fromto(0, 30, 2))
      val dedicatedF = []
  end

  val signedGCTest = false
  val addressWidth = 32
end

