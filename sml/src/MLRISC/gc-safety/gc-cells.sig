(*
 * This module provides few helper functions for annotating virtual registers
 * with gc type information. 
 *)

signature GC_CELLS =
sig

   structure C  : CELLS
   structure GC : GC_TYPE

   (* Generate a virtual register and update the gc info at the same time. *)
   val newCell   : C.cellkind -> GC.gctype -> C.cell
   val setGCType : C.cell * GC.gctype -> unit
   val getGCType : C.cell -> GC.gctype

   (* Prettty print gc type *)
   val printType : C.cell -> string

   val GCLIVEOUT : (C.cell * GC.gctype) list Annotations.property

end
