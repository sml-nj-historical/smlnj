(* graph-scc-sig.sml
 *
 * COPYRIGHT (c) 1999 Lucent Bell Laboratories.
 *
 *   Calculate strongly-connected components of directed graph.
 *   The graph can have nodes with self-loops.
 *
 * author: Matthias Blume
 *) 

signature GRAPH_SCC =
  sig

    structure Nd : ORD_KEY

    type node = Nd.ord_key

    datatype component
      = SIMPLE of node			(* singleton, no self-loop *)
      | RECURSIVE of node list

    val topOrder: { root: node, follow: node -> node list } -> component list
	(* take root node and follow function and return
	 * list of topologically sorted strongly-connected components;
	 * root component goes first
	 *)

  end

