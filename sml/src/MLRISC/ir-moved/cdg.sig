signature CONTROL_DEPENDENCE_GRAPH =
sig

    structure Dom : DOMINATOR_TREE

    type ('n,'e,'g) cdg = ('n,'e,'g) Graph.graph

    val control_dependence_graph : 
          ('e -> bool) ->
          ('n,'e,'g) Dom.dominator_tree * 
          ('n,'e,'g) Dom.postdominator_tree ->
          ('n,'e,'g) cdg 

    val control_dependence_graph' : 
          ('n Graph.node -> 'n2 Graph.node) ->
          ('e Graph.edge -> 'e2 Graph.edge) ->
          ('g -> 'g2) ->
          ('e -> bool) ->
          ('n,'e,'g) Dom.dominator_tree * 
          ('n,'e,'g) Dom.postdominator_tree ->
          ('n2,'e2,'g2) cdg 

end

(*
 * $Log: cdg.sig,v $
 * Revision 1.1.1.1  1998/11/16 21:48:47  george
 *   Version 110.10
 *
 *)
