(* cells.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * CELLS - describes storage units on the machine, such as
 *         dedicated and general registers, memory ...
 *
 *	 This file acts as a bridge between MLRISC and the machine 
 *	 code.
 *
 *)
signature CELLS = sig

  val stackptrR : int			(* stack pointer register *)
  val asmTmpR : int			(* assembly temporary *)
  val fasmTmp : int			(* floating point temporary *)

  val newReg    : unit -> int
  val newFreg   : unit -> int
  val newCCreg  : unit -> int
  val maxReg    : unit -> int
  val maxFreg   : unit -> int
  val numRegs   : unit -> int
  val numFregs  : unit -> int

  val firstPseudoReg  : int

  val resetRegs : unit -> int Intmap.intmap
     (* reset any local state *)

  type cellset
  val cellset2string : cellset -> string
  val addReg	     : int * cellset -> cellset
  val addFreg	     : int * cellset -> cellset
  val addCCreg       : int * cellset -> cellset
  val empty	     : cellset
  val cellsetToRegs  : int Intmap.intmap * cellset -> int list
end


(*
 * $Log: cells.sig,v $
 * Revision 1.4  1998/02/17 02:51:56  george
 *   Added cellsetToRegs -- a mapping of cells to unique ids.
 *
 * Revision 1.3  1997/07/17 12:28:43  george
 *   The regmap is now represented as an int map rather than using arrays.
 *
# Revision 1.2  1997/07/03  13:54:49  george
#   Added reserved floating point temporary -- fasmTmp
#
# Revision 1.1.1.1  1997/04/19  18:14:20  george
#   Version 109.27
#
 *)
