(*
 *  Build a DDG from a basic block
 *)
functor BasicBlockSchedulerDDGBuilder
   (structure DDG        : SCHEDULER_DDG
    structure InsnProps  : INSN_PROPERTIES
      sharing DDG.I = InsnProps.I
   ) : BASIC_BLOCK_SCHEDULER_DDG_BUILDER =
struct

   structure DDG        = DDG
   structure I          = DDG.I
   structure C          = I.C
   structure SchedProps = DDG.SchedProps
   structure HA         = HashArray
   structure G          = Graph

   type architecture = string
   type ddg = (I.instruction,DDG.latency) DDG.ddg

   fun error msg = MLRiscErrorMsg.error("BasicBlockSchedulerDDGBuilder",msg)

   val COPY_LATENCY = 0
  (*
   * Build a DAG from a list of instructions (in reverse order)
   * This is just a simple def/use analysis.
   *)
   fun buildDDG{regmap,cpu_info,ddg=G.GRAPH ddg} =
   let val SchedProps.CPU_INFO{defUse,...} = cpu_info
       fun buildDAG insns =
       let val defMap = HA.array(31,[]) and useMap = HA.array(31,[]) 
           fun flowDep i (r,latency) = 
               app (fn j => #add_edge ddg (i,j,latency)) (HA.sub(useMap,r))
           fun outputDep i (r,_) = 
               app (fn j => #add_edge ddg (i,j,~1)) (HA.sub(defMap,r))
           fun antiDep i r = 
               app (fn j => #add_edge ddg (i,j,~1)) (HA.sub(defMap,r))
           fun ctrlDep i j = #add_edge ddg (i,j,~1)
           fun addDefs n (r,l) = (HA.remove(useMap,r); HA.update(defMap,r,[n]))
           fun addUses n r = HA.update(useMap,r,n::HA.sub(useMap,r))

           fun copyDstSrc i' =
           let val (dst, src) = InsnProps.moveDstSrc i'
               fun coalesce(d::ds, s::ss, dst, src) = 
                   let val d = regmap d and s = regmap s
                   in  if d = s then coalesce(ds, ss, dst, src)
                       else coalesce(ds, ss, (d,COPY_LATENCY)::dst, s::src)
                   end
                 | coalesce([], [], dst, src) = (dst, src)
                 | coalesce _ = error "coalesce"

               val (dst, src) = coalesce(dst, src, [], [])
               val dst = case InsnProps.moveTmpR i' of
                           NONE => dst
                         | SOME tmp => (regmap tmp,~1)::dst
           in  (dst, src) end

           fun scan(i,[],branches,succs) = ()
             | scan(i,i'::insns,branches,succs) = 
           let val _    = #add_node ddg (i,i') (* create node *)
               val kind = InsnProps.instrKind i' 
               val (defs,uses) = 
                   case kind of
                     InsnProps.IK_COPY => copyDstSrc i'
                   | _ => let val (d, u) = defUse i'
                          in  (map (fn (r,l) => (regmap r,l)) d, map regmap u) 
                          end
               val _ = #add_node ddg (i,i')
               val _ = app (flowDep i) defs
               val _ = app (outputDep i) defs
               val _ = app (antiDep i) uses
               val _ = app (ctrlDep i) branches
               val _ = app (addDefs i) defs
               val _ = app (addUses i) uses
               val branches = 
                   case kind of
                     InsnProps.IK_JUMP => [i]
                   | InsnProps.IK_CALL => (app (ctrlDep i) succs; [i])
                   | _  => branches
           in  scan(i+1,insns,branches,i::succs)
           end
       in  scan(0,insns,[],[])
       end
   in  buildDAG
   end

end
