signature GRAPH_TOPOLOGICAL_SORT = 
sig

      (* topological sort *)

   val topsort : ('n,'e,'g) Graph.graph -> 
		    Graph.node_id list -> Graph.node_id list

end

(* 
 * $Log: graph-topsort.sig,v $
 * Revision 1.1.1.1  1998/11/16 21:48:35  george
 *   Version 110.10
 *
 *)
