(*
 * Description of registers and other updatable cells.
 *
 * IMPORTANT NOTE: 
 * All physical registers in the machine architecture 
 * all given unique encodings.  The encoding is not necessarily zero based.
 * For example, 0 may NOT represent floating point register 0. 
 *
 * This means that the client should not
 * use hard coded integers to represent physical registers, 
 * but should instead use the function:
 *
 *    Reg : cellkind -> int -> register
 *
 * to compute the proper encoding.
 *
 * A call "Reg k n" returns the nth physical register of kind k.
 * For integer and float point registers, the functions:
 *
 *   GPReg : int -> register
 *   FPReg : int -> register
 *
 * can also be used as shortcuts.
 *
 * -- Allen.
 *) 
signature CELLS_BASIS = 
sig
   eqtype cellkind 
   type register = int
   type regmap   = register Intmap.intmap
   exception Cells

   val cellkinds : cellkind list  (* list of all the cellkinds *)

   val cellkindToString : cellkind -> string

       (* first pseudo register *)
   val firstPseudo : register                    

       (* returns the encoding for the nth physical register of the given kind,
        * raises Cells if there is none.
        *)
   val Reg   : cellkind -> int -> register
   val GPReg : int -> register (* Reg GP *)
   val FPReg : int -> register (* Reg FP *)

       (* given a cellkind returns its encoding range *)
   val cellRange : cellkind -> {low:int, high:int}

       (* generate a new name for a virtual register *)
   val newCell   : cellkind -> unit -> register 

       (* lookup the cellkind of a virtual register *)
   val cellKind : register -> cellkind         

       (* update the cellkind of a virtual register *)
   val updateCellKind : register * cellkind -> unit        

       (* lookup the number of virtual registers in a cellkind *)
   val numCell   : cellkind -> unit -> int              

       (* the next virtual register name *) 
   val maxCell   : unit -> register
     
       (* newCell GP *)
   val newReg    : unit -> register              

       (* newCell FP *)
   val newFreg   : unit -> register              

       (* Create a new register that has the same cellkind as the given one 
        * Note: the numCell kind is NOT updated!
        *)
   val newVar    : register -> register

       (* create a new regmap *)
   val regmap    : unit -> regmap
   val lookup    : regmap -> register -> register

       (* reset all counters *)
   val reset     : unit -> unit

        (* auxiliary functions *)
   val printSet : (register -> string) -> (register list -> string)
   val printTuple : string list * string list -> string
end

(*
 * This is the abstract interface of cells
 *)
signature CELLS = 
sig
   include CELLS_BASIS
   val GP   : cellkind  (* general purpose *)
   val FP   : cellkind  (* floating point *)
   val CC   : cellkind  (* condition code *)
   val MEM  : cellkind  (* memory cell *)
   val CTRL : cellkind  (* control dependence *)
   val toString : cellkind -> register -> string
   val stackptrR : register                    (* stack pointer register *)
   val asmTmpR : register                      (* assembly temporary *)
   val fasmTmp : register                      (* floating point temporary *)
   val zeroReg : cellkind -> register option   (* register that contains 0 *)

   type cellset

      (* building a cellset *)
   val empty      : cellset
   val addCell    : cellkind -> register * cellset -> cellset
   val addReg     : register * cellset -> cellset
   val addFreg    : register * cellset -> cellset
   val getCell    : cellkind -> cellset -> register list
   val updateCell : cellkind -> cellset * register list -> cellset

       (* pretty printing, the second one takes a regmap *)
   val cellsetToString : cellset -> string
   val cellsetToString' : (register -> register) -> cellset -> string

       (* convert cellset into a list of registers *)
   val cellsetToRegs : cellset -> register list
end
