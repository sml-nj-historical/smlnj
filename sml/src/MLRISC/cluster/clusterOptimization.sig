(*
 * Signature of a cluster-based optimization phase
 *) 
signature CLUSTER_OPTIMIZATION =
sig
   structure F : FLOWGRAPH
   include MLRISC_OPTIMIZATION
     where type flowgraph = F.cluster
end
