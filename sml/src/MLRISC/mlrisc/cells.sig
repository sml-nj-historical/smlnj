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

  type regmap = int Intmap.intmap
  eqtype cellclass
  exception Cells
  val GP   : cellclass  (* general purpose register *)
  val FP   : cellclass  (* floating point register *)
  val CC   : cellclass  (* conditional code register *)
  val MEM  : cellclass  (* memory *)
  val CTRL : cellclass  (* control dependence *)

  val stackptrR : int			(* stack pointer register *)
  val asmTmpR : int			(* assembly temporary *)
  val fasmTmp : int			(* floating point temporary *)

  val newCell : cellclass -> unit -> int (* generate a new name *)
  val numCell : cellclass -> unit -> int (* number of names in class *)
  val maxCell : unit -> int		 (* max id of name *)
  val cellToString : int * cellclass -> string

  val newReg : unit -> int		(* newClass GP *)
  val newFreg : unit -> int		(* newClass FP *)
  val newCCreg : unit -> int		(* newClass CC *)

  val firstPseudo : int
  val zero : cellclass -> int option 
       (* name of the register that contains zero *)

  val resetRegs : unit -> regmap (* reset any local state *)

  type cellset
  val cellset2string : cellset -> string
  val empty	     : cellset
  val addCell        : cellclass -> int * cellset -> cellset
  val cellsetToRegs  : regmap * cellset -> int list

  val addReg  : int * cellset -> cellset (* addCell GP *)
  val addFreg : int * cellset -> cellset (* addCell FP *)
end


(*
 * $Log: cells.sig,v $
 * Revision 1.2  1998/05/19 15:47:05  george
 *   The cells interface now makes registers an abstract type called cellclass.
 *
 * Revision 1.1.1.1  1998/04/08 18:39:02  george
 * Version 110.5
 *
 *)
