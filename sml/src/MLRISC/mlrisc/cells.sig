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
 * Revision 1.1.1.1  1998/04/08 18:39:02  george
 * Version 110.5
 *
 *)
