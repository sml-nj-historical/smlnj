(*
 * Compute liveness information from an SSA graph.
 *)
signature SSA_LIVENESS =
sig

   structure SSA : SSA

   (* Compute live out information for each basic block in the SSA *)
   val liveOut : SSA.ssa -> RegSet.regset Array.array

   (* Is variable v live out at block b? *)
   val isLiveOut : SSA.ssa -> {v:SSA.value,b:SSA.block} -> bool

end

(*
 * $Log: ssa-liveness.sig,v $
 * Revision 1.1.1.1  1998/11/16 21:47:06  george
 *  Version 110.10
 *
 *)
