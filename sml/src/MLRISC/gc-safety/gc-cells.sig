(*
 * This module provides few helper functions for annotating virtual registers
 * with gc type information. 
 *)

signature GC_CELLS =
sig

   structure C  : CELLS
   structure GC : GC_TYPE
   structure GCMap : GC_MAP
      sharing GCMap.GC = GC

   val newGCMap : unit -> GCMap.gcmap  (* create a new gc map *)
   val setGCMap : GCMap.gcmap -> unit  (* set the current gc map *)
   val getGCMap : unit -> GCMap.gcmap  (* get the current gc map *)
   val clearGCMap : unit -> unit       (* remove current gc map *) 

   (*
    * Generate a virtual register and update the gc map at the same time.
    * Important note: the gc map must be set by setGCMap before this 
    * function can be invoked.
    *)
   val newCell  : C.cellkind -> GC.gctype -> C.cell

end
