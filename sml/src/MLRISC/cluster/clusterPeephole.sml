(*
 * Run peephole optimization on a cluster
 *)
functor ClusterPeephole
  (structure F        : FLOWGRAPH
   structure PeepHole : PEEPHOLE
     sharing F.I = PeepHole.I
  ) : CLUSTER_OPTIMIZATION =
struct
   structure F = F
   type flowgraph = F.cluster

   val name = "Peephole optimization"

   fun run(cluster as F.CLUSTER{blocks, regmap, ...}) =
   let val peephole = PeepHole.peephole (F.I.C.lookup regmap)
   in  app (fn F.BBLOCK{insns, ...} => insns := peephole(!insns)
             | _ => ()) blocks;
       cluster
   end
end
