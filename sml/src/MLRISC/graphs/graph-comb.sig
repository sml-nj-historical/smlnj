signature GRAPH_COMBINATIONS = 
sig

   (* disjoint union *)
   val sum   : ('n,'e,'g) Graph.graph * ('n,'e,'g) Graph.graph ->
                  ('n,'e,'g) Graph.graph
   val union : ('n,'e,'g) Graph.graph list -> ('n,'e,'g) Graph.graph
   val sums  : ('n,'e,'g) Graph.graph list -> ('n,'e,'g) Graph.graph

end

(* 
 * $Log: graph-comb.sig,v $
 * Revision 1.1.1.1  1998/11/16 21:48:35  george
 *   Version 110.10
 *
 *)
