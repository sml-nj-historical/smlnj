(*
 * This gives a cluster a graph view so that all graph based algorithms
 * can be applied on the cluster.  The view is readonly though. 
 *
 * -- Allen
 *)
signature CLUSTER_GRAPH =
sig

   structure F : FLOWGRAPH
   structure W : FREQ
     sharing F.W = W

   type clusterInfo

   type clusterGraph = (F.block,W.freq ref,clusterInfo) Graph.graph

   val clusterGraph : F.cluster -> clusterGraph

   val cluster       : clusterGraph -> F.cluster
   val table         : clusterGraph -> F.block Array.array
   val isTakenBranch : W.freq ref Graph.edge -> bool
   val nodeFreq      : F.block -> W.freq ref

end
