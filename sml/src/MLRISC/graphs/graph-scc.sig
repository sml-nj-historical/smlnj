(*
 * This module computes strongly connected components (SCC) of
 * a graph.  Each SCC is represented as a list of nodes.  All nodes
 * are folded together with a user supplied function.
 *
 * -- Allen
 *)

signature GRAPH_STRONGLY_CONNECTED_COMPONENTS = 
sig

      (* strongly connected components *)

   val strong_components : ('n,'e,'g) Graph.graph -> 
		    (Graph.node_id list * 'a -> 'a) -> 'a -> 'a

end

