signature PPCCELLS = sig
  exception Cells
  type register = int
  type regmap = int Intmap.intmap

  datatype cellclass =
      (* required components*)
      GP | FP | CC| MEM | CTRL	
      (* ppc specific *)
    | LR | CTR 
 
  val stackptrR : int
  val asmTmpR : int
  val fasmTmp : int
  val lr : int

  val newCell : cellclass -> unit -> int (* generate a new name *)
  val numCell : cellclass -> unit -> int (* number of names in class *)
  val maxCell : unit -> int		 (* max id of name *)
  val cellToString : int * cellclass -> string

  val newReg : unit -> int 	(* newClass GP *)
  val newFreg : unit -> int	(* newClass FP *)
  val newCCreg : unit -> int	(* newClass CC *)

  val firstPseudo : int
  val zero : cellclass -> int option 
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