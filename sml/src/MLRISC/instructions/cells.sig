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
 *    Reg : cellkind -> int -> cell
 *
 * to compute the proper encoding.
 *
 * A call "Reg k n" returns the nth physical register of kind k.
 * For integer and float point registers, the functions:
 *
 *   GPReg : int -> cell
 *   FPReg : int -> cell
 *
 * can also be used as shortcuts.
 *
 * -- Allen.
 *) 
signature CELLS_BASIS = 
sig
   eqtype cellkind 
   type cell = int
   type ty   = int
   type regmap = cell Intmap.intmap
   exception Cells

   val cellkinds : cellkind list  (* list of all the cellkinds *)

   val cellkindToString : cellkind -> string

       (* first pseudo register *)
   val firstPseudo : cell                    

       (* returns the encoding for the nth physical register of the given kind,
        * raises Cells if there is none.
        *)
   val Reg   : cellkind -> int -> cell
   val GPReg : int -> cell (* Reg GP *)
   val FPReg : int -> cell (* Reg FP *)

       (* given a cellkind returns its encoding range *)
   val cellRange : cellkind -> {low:int, high:int}

       (* generate a new name for a virtual register *)
   val newCell   : cellkind -> 'a -> cell 

       (* lookup the number of virtual registers in a cellkind *)
   val numCell   : cellkind -> unit -> int              

       (* the next virtual register name *) 
   val maxCell   : unit -> cell
     
       (* newCell GP *)
   val newReg    : 'a -> cell              

       (* newCell FP *)
   val newFreg   : 'a -> cell              

       (* Create a new register that has the same cellkind as the given one 
        * Note: the numCell kind is NOT updated!
        *)
   val newVar    : cell -> cell

       (* create a new regmap *)
   val regmap    : unit -> regmap
   val lookup    : regmap -> cell -> cell

       (* reset all counters *)
   val reset     : unit -> unit

        (* auxiliary functions *)
   val printSet : (cell -> string) -> (cell list -> string)
   val printTuple : string list * string list -> string

    (*
     * These annotations adds extract definitions and uses to an instruction
     *)
   exception DEF_USE of {cellkind:cellkind, defs:cell list, uses:cell list}
   val DEFUSE : {cellkind:cellkind, defs:cell list, uses:cell list}
                  Annotations.property
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
   val toString : cellkind -> cell -> string
   val toStringWithType : cellkind -> cell * ty -> string
   val stackptrR : cell                    (* stack pointer register *)
   val asmTmpR : cell                      (* assembly temporary *)
   val fasmTmp : cell                      (* floating point temporary *)
   val zeroReg : cellkind -> cell option   (* register that contains 0 *)

   type cellset

      (* building a cellset *)
   val empty      : cellset
   val addCell    : cellkind -> cell * cellset -> cellset
   val rmvCell    : cellkind -> cell * cellset -> cellset
   val addReg     : cell * cellset -> cellset
   val rmvReg     : cell * cellset -> cellset
   val addFreg    : cell * cellset -> cellset
   val rmvFreg    : cell * cellset -> cellset
   val getCell    : cellkind -> cellset -> cell list
   val updateCell : cellkind -> cellset * cell list -> cellset

       (* pretty printing, the second one takes a regmap *)
   val cellsetToString : cellset -> string
   val cellsetToString' : (cell -> cell) -> cellset -> string

       (* convert cellset into a list of cells *)
   val cellsetToCells : cellset -> cell list
end
