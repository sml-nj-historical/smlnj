(*
 * This module takes a stream of instructions and build a CFG.
 * The building can be incremental.
 *
 * -- Allen
 *)
functor ControlFlowGraphGenFn
   (structure CFG     : CONTROL_FLOW_GRAPH
    structure Stream  : INSTRUCTION_STREAM
    structure InsnProps : INSN_PROPERTIES
        sharing CFG.I = InsnProps.I
        sharing CFG.P = Stream.P
        sharing CFG.B = Stream.B
   ) : CONTROL_FLOW_GRAPH_GEN =
struct

   structure CFG     = CFG
   structure Props   = InsnProps
   structure I       = CFG.I
   structure B       = CFG.B
   structure P       = CFG.P
   structure G       = Graph
   structure W       = CFG.W
   structure S       = Stream

   fun error msg = MLRiscErrorMsg.error("ControlFlowGraphGen",msg)

   fun warning msg = print("Warning: "^msg^"\n")

   fun builder(CFG) = 
   let val currentBlock = ref NONE : CFG.block option ref
       val newBlocks    = ref [] : CFG.block list ref 
       val blkName      = ref B.default
       exception NotFound
       val labelMap = Intmap.new(43,NotFound)
       val newLabel = Intmap.add labelMap
       val CFG = ref CFG

       fun init _ =
           (currentBlock := NONE;
            newBlocks := [];
            blkName := B.default; 
            Intmap.clear labelMap;
            CFG.init(!CFG)
           ) 

       val _ = init()

       fun next cfg = (CFG := cfg; init())

       fun newBlock() = 
             let val G.GRAPH cfg = !CFG
                 val id = #new_id cfg ()
                 val b  = CFG.newBlock(id,!blkName,ref 0)
             in  currentBlock := SOME b; 
                 newBlocks := b :: !newBlocks;
                 #add_node cfg (id,b);
                 b 
             end

       fun getBlock() = 
           case !currentBlock of
              NONE   => newBlock()
           |  SOME b => b

       fun newPseudoOpBlock() =
            (case !currentBlock of
                SOME(b as CFG.BLOCK{insns=ref [],...}) => b
             |  _ => newBlock()
            )  

       fun insertOp p = 
            let val CFG.BLOCK{data,...} = newPseudoOpBlock()
            in  data := !data @ [p] end

       fun defineLabel(l as Label.Label{id,...}) = 
           let val b as CFG.BLOCK{labels,...} = newPseudoOpBlock()
           in  labels := l :: !labels;
               newLabel(id,b)
           end

       fun pseudoOp p = insertOp(CFG.PSEUDO p)

       fun exitBlock liveOut = 
           let val CFG.BLOCK{annotations,...} = getBlock()
           in  annotations := CFG.LIVEOUT liveOut :: !annotations
           end

       fun comment msg = 
           let val CFG.BLOCK{annotations,...} = getBlock()
           in  annotations := BasicAnnotations.COMMENT msg :: !annotations
           end

       fun annotation a = 
           let val CFG.BLOCK{annotations,...} = getBlock()
           in  annotations := a :: !annotations
           end

       fun blockName name = blkName := name

       fun entryLabel(l as Label.Label{id,...}) = 
       let val G.GRAPH cfg = !CFG
           val b as CFG.BLOCK{id=j,labels,...} = newPseudoOpBlock()
           val ENTRY = case #entries cfg () of
                          [ENTRY] => ENTRY
                       |  _ => raise Graph.NotSingleEntry
       in  labels := l :: !labels;
           newLabel(id,b);
           #add_edge cfg (ENTRY,j,CFG.EDGE{k=CFG.ENTRY,a=ref [],w=ref 0})
       end

       fun emitInstr i =
           let val CFG.BLOCK{insns,...} = getBlock()
           in  insns := i :: !insns;
               if Props.instrKind i = Props.IK_JUMP then
                  currentBlock := NONE
               else () 
           end

       fun finish(regmap,annotations) =
       let val G.GRAPH cfg = !CFG
           val EXIT = case #exits cfg () of
                        [EXIT] => EXIT
                      | _ => raise Graph.NotSingleExit
           fun nextBlock(CFG.BLOCK{id,data=ref [],...}::_) = id
             | nextBlock _ = error "nextBlock"
           fun target (Label.Label{id,...}) = 
               let val CFG.BLOCK{id,...} = Intmap.map labelMap id
               in  id end
           fun addEdges [] = ()
             | addEdges(CFG.BLOCK{id,insns,...}::blocks) =
               (case !insns of
                  [] => fallsThru(id,blocks)
                | instr::_ =>
                   if Props.instrKind instr = Props.IK_JUMP then
                      jump(id,Props.branchTargets instr,blocks)
                   else 
                     fallsThru(id,blocks);
                addEdges blocks
               )
           and fallsThru(i,CFG.BLOCK{id=j,data,...}::_) =
                 (case !data of
                     [] => ()
                  |  _  => warning("falls thru into pseudo ops: "^
                                   Int.toString i^" -> "^Int.toString j)
                  ;
                  #add_edge cfg (i,j,CFG.EDGE{k=CFG.FALLSTHRU,
                                              w=ref 0, a=ref []
                                             })
                 )
             | fallsThru(i,[]) =
                  error("missing return in block "^Int.toString i)
           and jump(i,[Props.ESCAPES],_) =
                   #add_edge cfg (i,EXIT,CFG.EDGE{k=CFG.EXIT,
                                                  w=ref 0,a=ref []
                                                 })
             | jump(i,[Props.LABELLED L],_) =
                  #add_edge cfg (i,target L,CFG.EDGE{k=CFG.JUMP,
                                                     w=ref 0, a=ref []})
             | jump(i,[Props.LABELLED L,Props.FALLTHROUGH],bs) =
                  (#add_edge cfg (i,target L,CFG.EDGE{k=CFG.BRANCH true,
                                                      w=ref 0, a=ref []
                                                     });
                   #add_edge cfg (i,nextBlock bs,CFG.EDGE
                                                     {k=CFG.BRANCH false,
                                                      w=ref 0, a=ref []
                                                     })
                  )
             | jump(i,targets,_) =
                  let fun f(n,[]) = ()
                        | f(n,Props.LABELLED L::targets) =
                        (#add_edge cfg (i,target L,CFG.EDGE
                                                   {k=CFG.SWITCH n,
                                                    w=ref 0, a=ref []});
                         f(n+1,targets))
                        | f _ = error "jump"
                  in  f(0,targets) end
          in  addEdges(rev(!newBlocks));
              CFG.setRegmap(!CFG,regmap);
              CFG.setAnnotations(!CFG,annotations);
              init()
          end

    in  {stream=S.STREAM
           {  init        = init,
              defineLabel = defineLabel,
              entryLabel  = entryLabel,
              pseudoOp    = pseudoOp,
              emit        = fn _ => emitInstr,
              exitBlock   = exitBlock,
              blockName   = blockName,
              comment     = comment,
              annotation  = annotation,
              finish      = finish
           },
         next = next
        }
    end  

end

