(* omit the frame pointer based by rewriting to use the stack pointer. *)

signature OMIT_FRAME_POINTER = sig
  structure F : FLOWGRAPH
  structure I : INSTRUCTIONS
  
  (* idelta is the intial displacement between the fp and sp. *)
  val omitframeptr : {vfp:CellsBasis.cell, idelta:Int32.int option, cl:F.cluster} -> unit
end
