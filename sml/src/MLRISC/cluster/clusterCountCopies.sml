(*
 * This module counts the number of copies (in bytes)  
 * generated after register allocation.  Mainly useful for fine-tuning.
 *)
functor ClusterCountCopies
   (structure F : FLOWGRAPH
    structure InsnProps : INSN_PROPERTIES
    structure SdiJumps : SDI_JUMPS
      sharing F.I = InsnProps.I = SdiJumps.I
   ) : CLUSTER_OPTIMIZATION =
struct
   structure F = F
   type flowgraph = F.cluster

   val name = "count copies"

   val copies = MLRiscControl.getCounter "copies"

   fun run(cluster as F.CLUSTER{blocks, regmap, ...}) =
   let val regmap = F.I.C.lookup regmap
       fun loc _ = 0

       fun (* count(F.BBLOCK{freq=ref 0, ...}, n) = n
         | *) count(F.BBLOCK{insns, ...}, n) = 
           let fun scan([], n) = n
                 | scan(i::is, n) = 
                   if InsnProps.moveInstr i then
                      scan(is, n + SdiJumps.sdiSize(i, regmap, loc, 0)) 
                   else scan(is, n)
           in  scan(!insns, n) end
         | count(_, n) = n
   in  copies := !copies + foldr count 0 blocks;
       cluster
   end
end
