(* X86Cells.sig
 *
 * COPYRIGHT (c) 1999 Bell Laboratories.
 *)

signature X86CELLS = sig

  exception Cells
  type register = int
  type regmap = register Intmap.intmap
  datatype cellclass = GP | FP | CC | MEM | CTRL

  val stackptrR : int			(* stack pointer register *)
  val asmTmpR : int			(* assembly temporary *)
  val fasmTmp : int			(* floating point temporary *)

  val eax : int
  val ecx : int
  val edx : int
  val ebx : int
  val esp : int
  val esi : int
  val ebp : int
  val edi : int

  val newCell : cellclass -> unit -> register (* generate a new name *)
  val numCell : cellclass -> unit -> register (* number of names in class *)
  val maxCell : unit -> int		 (* max id of name *)
  val cellToString : register * cellclass -> string

  val newReg : unit -> register		(* newClass GP *)
  val newFreg : unit -> register	(* newClass FP *)
  val newCCreg : unit -> register	(* newClass CC *)

  val firstPseudo : register
  val zero : cellclass -> register option 
       (* name of the register that contains zero *)

  val resetRegs : unit -> regmap (* reset any local state *)

  type cellset
  val cellset2string : cellset -> string
  val empty	     : cellset
  val addCell        : cellclass -> register * cellset -> cellset
  val cellsetToRegs  : regmap * cellset -> register list

  val addReg  : register * cellset -> cellset (* addCell GP *)
  val addFreg : register * cellset -> cellset (* addCell FP *)
end
