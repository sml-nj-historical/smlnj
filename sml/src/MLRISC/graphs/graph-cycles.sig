signature GRAPH_SIMPLE_CYCLES = 
sig

      (* enumerate all simple cycles *)

   val cycles : ('n,'e,'g) Graph.graph -> 
		    ('e Graph.edge list * 'a -> 'a) -> 'a -> 'a

end

(* 
 * $Log: graph-cycles.sig,v $
 * Revision 1.1.1.1  1998/11/16 21:48:36  george
 *   Version 110.10
 *
 *)
