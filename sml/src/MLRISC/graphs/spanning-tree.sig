signature MIN_COST_SPANNING_TREE =
sig

   exception Unconnected

   val spanning_tree : { weight    : 'e Graph.edge -> 'w,
                         <         : 'w * 'w -> bool
                       } -> ('n, 'e, 'g) Graph.graph 
                         -> ('e Graph.edge * 'x -> 'x) -> 'x -> 'x
end

(*
 * $Log: spanning-tree.sig,v $
 * Revision 1.1.1.1  1998/11/16 21:48:36  george
 *   Version 110.10
 *
 *)
