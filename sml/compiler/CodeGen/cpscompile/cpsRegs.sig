(* cpsRegs.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * The registers used for CPS compilation.
 *
 *)

signature CPSREGS =
  sig
    structure T : MLTREE
    structure C : CELLS

    val vfp		: CellsBasis.cell
    val vfptr		: T.rexp

    val allocptr 	: T.rexp	(* must be a hardware register, - T.REG(r) *)

  (* The boolean argument in each case indicates the use of the virtual
   * frame pointer. Use virtual fp if true and physical fp if false.
   *
   * In principle a lot more of these should be functions over the boolean,
   * however, the x86 is the only one that implements registers in memory,
   * so we will limit this to the set that it needs. 
   *)
    val frameptr	: bool -> T.rexp
    val limitptr	: bool -> T.rexp
    val stdlink		: bool -> T.rexp
    val stdclos		: bool -> T.rexp
    val stdarg 		: bool -> T.rexp 
    val stdcont 	: bool -> T.rexp 
    val exnptr 		: bool -> T.rexp 
    val varptr  	: bool -> T.rexp 
    val baseptr		: bool -> T.rexp
    val storeptr 	: bool -> T.rexp 
    val gcLink		: bool -> T.rexp 
  
    val calleesave	: T.rexp Array.array
    val exhausted 	: T.ccexp option
    val signedGCTest 	: bool
    val addressWidth 	: T.ty

    val miscregs  	: T.rexp list
    val floatregs 	: T.fexp list
    val savedfpregs 	: T.fexp list

    val dedicatedR 	: T.reg list
    val availR     	: T.reg list
    val dedicatedF 	: T.reg list
    val availF     	: T.reg list

    val ccallCallerSaveR : T.reg list
    val ccallCallerSaveF : T.reg list

  (* number of bits and bytes per ML word *)
    val wordBitWidth	: T.ty
    val wordByteWidth	: int

  end
