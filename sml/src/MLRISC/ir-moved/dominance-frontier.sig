signature DOMINANCE_FRONTIERS =
sig

   structure Dom : DOMINATOR_TREE

   type dominance_frontiers = Graph.node_id list Array.array

   val DFs : ('n,'e,'g) Dom.dominator_tree -> dominance_frontiers

end

(*
 * $Log: dominance-frontier.sig,v $
 * Revision 1.1.1.1  1998/11/16 21:48:47  george
 *   Version 110.10
 *
 *)
