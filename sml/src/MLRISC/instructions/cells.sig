(*
 * This updated signature describes the abstractions on ``cells'', which
 * denote storage cells in the machine architecture.
 *
 * Allen Leung (12/2/00)
 *)
(*
 * Things that are architecture specific.
 *)
signature CELLS_COMMON = 
sig
   include CELLS_BASIS
   structure CellsBasis : CELLS_BASIS = CellsBasis
         sharing type cellkind     = CellsBasis.cellkind   
             and type cellkindDesc = CellsBasis.cellkindDesc
             and type cellkindInfo = CellsBasis.cellkindInfo
             and type cell         = CellsBasis.cell
             and type HashTable.hash_table = CellsBasis.HashTable.hash_table
         sharing SortedCells = CellsBasis.SortedCells

   exception Cells

   val cellkinds : cellkind list  (* list of all the cellkinds *)

   val firstPseudo : cell_id      (* first pseudo register *)

       (* given a cellkind returns its encoding range *)
   val cellRange : cellkind -> {low:int, high:int}

       (* Returns the nth physical register of the given kind,
        * raises Cells if there are no physical register of the given number.
        * Also raises Cells if the given number if outside of the range.
        * NOTE: this function returns the same cell for the 
        * same argument every time.   See also the function cloneCell below
        *)
   val Reg   : cellkind -> (register_num -> cell)

       (* return a list of cells *)
   val Regs  : cellkind -> {from:register_num, to:register_num, step:int} ->
                 cell list

       (* Same as Reg but we take the id instead.
        * So, registerNum(Reg k r) = r, and
        *     registerId(Cell k id) = id
        *)
   val Cell  : cellkind -> (register_id -> cell) 

   val GPReg : int -> cell (* abbreviation for Reg GP *)
   val FPReg : int -> cell (* abbreviation for Reg FP *)

       (*
        * Generate a new cell for a virtual register.  The new cell
        * is a pseudo register that is distinct from any other registers.
        * IMPORTANT: if you are using newCell, it is important to 
        * partially apply it first to get a function.  Then uses this
        * function generate new cells.  The first application takes
        * time.
        *)
   val newCell   : cellkind -> ('a -> cell)
   val newReg    : 'a -> cell  (* abbreviation for newCell GP *)
   val newFreg   : 'a -> cell  (* abbreviation for newCell FP *)

       (* lookup the number of virtual registers in a cellkind *)
   val numCell   : cellkind -> (unit -> int) 

       (* the next virtual register name *) 
   val maxCell   : unit -> cell_id
     
       (* Given a cell c, create a new pseudo register that has the same 
        * cellkind as c, and a new property list initialized 
        * with the contents of c's properity list.
        * Note: the numCell kind is NOT updated!
        *)
   val newVar : cell -> cell

       (* This is the same as above, except that if the original
        * cell is colored, then the new cell has the same color.
        * Note that it is possible to have two cells (or more) with
        * the same physical color.  In these cases they can be used
        * to denote the same register, but they have different identities,   
        * and different property lists.  This may be useful for 
        * representing the same register used in different situations.  
        * See the function Reg above.
        *)
   val cloneCell : cell -> cell

       (* Reset all counters. *) 
   val reset     : unit -> unit 

   (* 
    * Cell set represents a map from cellkind to sorted_cells.
    *)
   structure CellSet :
   sig
      type cellset 
      (* cellset functions *)
      val empty      : cellset
      val add        : cell * cellset -> cellset
      val rmv        : cell * cellset -> cellset
      val get        : cellkind -> cellset -> cell list
      val update     : cellkind -> cellset * cell list -> cellset
      val map        : {from:cell,to:cell} -> cellset -> cellset

      (* convert cellset into a list of cells *)
      val toCellList : cellset -> cell list

      (* pretty printing *)
      val toString   : cellset -> string
   end

       (* Abbreviations for cellsets *)
   type cellset = CellSet.cellset 

   val empty      : cellset
   val getReg     : cellset -> cell list 
   val addReg     : cell * cellset -> cellset 
   val rmvReg     : cell * cellset -> cellset
   val getFreg    : cellset -> cell list 
   val addFreg    : cell * cellset -> cellset
   val rmvFreg    : cell * cellset -> cellset

       (* Return a register that is always zero on the architecture,
        * if one exists.  IMPORTANT: each call returns the same cell.
        * See also cloneCell above.
        *)
   val zeroReg    : cellkind -> cell option  
                           
   val defaultValues : cellkind -> (register_id * int) list

end

(*
 * This is the abstract interface of cells.
 *)
signature CELLS =
sig
   include CELLS_COMMON
   val stackptrR     : cell (* stack pointer register *)
   val asmTmpR       : cell (* assembly temporary *)
   val fasmTmp       : cell (* floating point temporary *)
end
