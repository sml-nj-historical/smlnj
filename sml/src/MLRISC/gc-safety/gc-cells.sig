(*
 * This module provides few helper functions for annotating virtual registers
 * with gc type information. 
 *)

signature GC_CELLS =
sig

   structure C  : CELLS
   structure GC : GC_TYPE

   val newGCMap : unit -> GC.gcmap  (* create a new gc map *)
   val setGCMap : GC.gcmap -> unit  (* set the current gc map *)
   val getGCMap : unit -> GC.gcmap  (* get the current gc map *)

   (*
    * Generate a virtual register and update the gc map at the same time.
    * Important note: the gc map must be set by setGCMap before this 
    * function can be invoked.
    *)
   val newCell  : C.cellkind -> GC.gctype -> C.cell

end
