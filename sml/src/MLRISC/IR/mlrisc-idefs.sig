(*
 * This is Reif and Tarjan's algorithm (SIAM J Computing 1981) 
 * for computing approximate birthpoints for expressions.   
 * For each basic block B,
 *   idef(B) = { v | v is defined on some path between B's idom and B }
 *
 * -- Allen
 *)
signature MLRISC_IDEFS =
sig

   structure Dom : DOMINATOR_TREE
   structure CFG : CONTROL_FLOW_GRAPH
   structure I   : INSTRUCTIONS
      sharing CFG.I = I

   val idefs : 
       (I.instruction -> I.C.cell list * I.C.cell list) ->
       CFG.cfg ->
       { idefuse     : unit -> (I.C.cell list * I.C.cell list) Array.array,
         ipostdefuse : unit -> (I.C.cell list * I.C.cell list) Array.array
       }
end

