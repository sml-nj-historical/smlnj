signature GRAPH_STRONGLY_CONNECTED_COMPONENTS = 
sig

      (* strongly connected components *)

   val scc : ('n,'e,'g) Graph.graph -> 
		    (Graph.node_id list * 'a -> 'a) -> 'a -> 'a

end

(* 
 * $Log: graph-scc.sig,v $
 * Revision 1.1.1.1  1998/11/16 21:48:35  george
 *   Version 110.10
 *
 *)
