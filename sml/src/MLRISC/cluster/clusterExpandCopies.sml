(*
 * This module expands all parallel copies into normal instructions
 *)
functor ClusterExpandCopies
   (structure Flowgraph    : FLOWGRAPH
    structure ExpandCopies : EXPAND_COPIES
       sharing Flowgraph.I = ExpandCopies.I
   ) : CLUSTER_OPTIMIZATION =
struct
   structure F = Flowgraph
   type flowgraph = F.cluster

   val name = "expand copies"

   fun run(cluster as F.CLUSTER{blocks, ...}) =
       (app (fn F.BBLOCK{insns, ...} =>
                insns :=  
                  List.foldr (fn (i, rest) =>
                      List.revAppend(ExpandCopies.expandCopies i,rest)) []
                        (!insns)
              | _ => ()) blocks;
        cluster
       )
end
