(* flowgen.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

functor ClusterGen
  (structure Flowgraph : FLOWGRAPH
   structure InsnProps : INSN_PROPERTIES
   structure MLTree : MLTREE
     sharing Flowgraph.I = InsnProps.I
     sharing MLTree.Constant = InsnProps.I.Constant  
     sharing MLTree.PseudoOp = Flowgraph.P 
  ) : FLOWGRAPH_GEN = 
struct

  structure F = Flowgraph
  structure I = Flowgraph.I
  structure C = I.C

  structure T = MLTree
  structure P = T.PseudoOp
  structure S = T.Stream
  
  fun error msg = MLRiscErrorMsg.error("ClusterGen",msg)

  fun can'tUse _ = error "unimplemented"

  type flowgraph = F.cluster

  (* This rewritten version allows increment flowgraph updates *)

  fun newStream{compile,flowgraph} = 
  let val NOBLOCK = F.LABEL(Label.Label{id= ~1, name="", addr=ref 0})

      val freq = 1

      val (blkCounter, regmap, annotations, blocks, entry, exit) = 
          case flowgraph of
            SOME(F.CLUSTER{blkCounter, regmap, annotations, blocks, 
                           entry, exit, ...}) =>
                  (ref(!blkCounter-2), 
                   ref regmap, !annotations, ref(rev blocks),
                   entry, exit)
          | NONE => (ref 0, ref(C.regmap()), [], ref [], NOBLOCK, NOBLOCK)

      val currBlock   = ref NOBLOCK
      val blockNames  = ref [] : Annotations.annotations ref
      val aliasF      = ref (IntHashTable.insert (!regmap))
      val entryLabels = ref [] : Label.label list ref

      fun nextBlockNum() = 
      let val n = !blkCounter in blkCounter := n + 1; n end

      (* Create a new basic block *)
      fun newBasicBlock insns =
      let val n = !blkCounter
      in  blkCounter := n + 1;
          F.BBLOCK{blknum      = n,
                   freq        = ref freq, 
                   annotations = ref (!blockNames),
                   liveIn      = ref C.empty,
                   liveOut     = ref C.empty,
                   succ        = ref [],
                   pred        = ref [],
                   insns       = ref insns
                  }
      end

      (* Add current block to the list *)
      fun endCurrBlock() =
          case !currBlock of
            blk as F.BBLOCK _ => (blocks := blk:: !blocks; currBlock := NOBLOCK)
          | _ => ()

      (* Add pseudo op/label to the block list *)
      fun blockListAdd b = (endCurrBlock(); blocks := b :: !blocks)
      fun pseudoOp pOp = blockListAdd (F.PSEUDO pOp)
      fun defineLabel lab = blockListAdd (F.LABEL lab)
      fun entryLabel lab = (entryLabels := lab :: !entryLabels; defineLabel lab)

      (* Add an instruction to the current block *)
      fun emit instr =
         (case !currBlock of
            F.BBLOCK{insns, ...} => insns := instr :: !insns
          | _ => currBlock := newBasicBlock [instr]
          ;
          case InsnProps.instrKind instr of 
            InsnProps.IK_JUMP => (blocks := !currBlock :: !blocks; 
                                  currBlock := NOBLOCK)
          | _ => ()
         )

      (* Add an annotation *)
      fun annotation a =
          case a of
            MLRiscAnnotations.BLOCKNAMES names =>
              (endCurrBlock(); blockNames := names)
          | MLRiscAnnotations.EMPTYBLOCK =>
              (case !currBlock of
                 F.BBLOCK _ => ()
               | _ => currBlock := newBasicBlock [];
               endCurrBlock()
              )
          | MLRiscAnnotations.EXECUTIONFREQ f =>
               (case !currBlock of
                  F.BBLOCK{freq, ...} => freq := f
                |  _ => (currBlock := newBasicBlock []; annotation a)
               )
          | a => (case !currBlock of
                     F.BBLOCK{annotations, ...} => 
                       annotations := a :: !annotations
                 |  _ => (currBlock := newBasicBlock []; annotation a)
                 )
 
      (* Add a comment *)
      fun comment msg = annotation(#create MLRiscAnnotations.COMMENT msg)

      (* Mark a block as exit *)
      fun exitBlock cellset =
      let fun findLiveOut(F.BBLOCK{liveOut, ...}::_) = liveOut
            | findLiveOut(F.LABEL _::blks) = findLiveOut blks
            | findLiveOut _ = error "exitBlock: no basic block"
      in  endCurrBlock();
          findLiveOut (!blocks) := cellset
      end

      (* Add an alias to the regmap *)
      fun alias(v,r) = !aliasF(v,r)

      (* Start a new cluster *)
      fun beginCluster _ = !regmap

      (* End a cluster *)
      fun endCluster blockAnnotations =
      let exception LabelMap
          val labelMap : F.block IntHashTable.hash_table =
	      IntHashTable.mkTable(16, LabelMap)
          val addLabelMap = IntHashTable.insert labelMap

          (* find the next code block *)
          fun nextCodeBlock((blk as F.BBLOCK _)::_) = blk
            | nextCodeBlock(_::blks) = nextCodeBlock blks
            | nextCodeBlock [] = error "nextCodeBlock"

          fun fillLabelMap(F.LABEL(Label.Label{id, ...})::blks,ids) =
              fillLabelMap(blks, id::ids)
            | fillLabelMap((blk as F.BBLOCK _)::blks, ids) =
              let fun loop [] = ()
                    | loop (id::ids) = (addLabelMap(id, blk); loop ids)
              in  loop ids; fillLabelMap(blks, [])  end
            | fillLabelMap(_::blks, ids) = fillLabelMap(blks, ids)
            | fillLabelMap([], _) = ()

          val exitBlk = 
              case exit of
                F.EXIT{freq, ...} => 
                     F.EXIT{blknum=nextBlockNum(), pred=ref [], freq=freq}
              | _ => F.EXIT{blknum=nextBlockNum(), pred=ref [], freq=ref freq}

          val (entryBlk, entryEdges) =
              case entry of
                F.ENTRY{freq, succ, ...} => 
                    (F.ENTRY{blknum=nextBlockNum(), succ=succ, freq=freq},
                     succ)
              | _ => 
                let val edges = ref []
                in  (F.ENTRY{blknum=nextBlockNum(), succ=edges, freq=ref freq},
                     edges)
                end

          fun lookupLabelMap i = getOpt (IntHashTable.find labelMap i, exitBlk)

          fun addPred blk (F.BBLOCK{pred, ...}, w) = pred := (blk,w) :: !pred
            | addPred blk (F.EXIT{pred, ...}, w) = pred := (blk,w) :: !pred
            | addPred _   _ = error "addPred"

          (* Update successor and predecessor edges *)
          fun insertGraphEdges [] = ()
            | insertGraphEdges((blk as F.BBLOCK{blknum,insns,succ,...})::rest) =
              let fun succBlocks([], succs) = succs
                    | succBlocks(InsnProps.FALLTHROUGH::labs, succs) = 
                        succBlocks(labs, (nextCodeBlock rest, ref 0)::succs)
                    | succBlocks(InsnProps.LABELLED(Label.Label{id,...})::labs,
                                 succs) =
                        succBlocks(labs, (lookupLabelMap id, ref 0)::succs)
                    | succBlocks(InsnProps.ESCAPES::labs, succs) =     
                        succBlocks(labs, (exitBlk, ref 0)::succs)

                  (* Is it the last code block *)
                  fun isLastCodeBlock(F.BBLOCK _::_) = false
                    | isLastCodeBlock(_::rest) = isLastCodeBlock rest
                    | isLastCodeBlock [] = true

              in  case !insns of
                    lastInstr::_ =>
                    (case InsnProps.instrKind lastInstr of
                       InsnProps.IK_JUMP => 
                          succ := succBlocks
                             (InsnProps.branchTargets lastInstr,[])
                     | _ => 
                       if isLastCodeBlock rest then
                          succ := [(exitBlk, ref 0)]
                                  (* control must escape via trap! *)
                       else succ := [(nextCodeBlock rest, ref 0)]
                    ) 
                  | [] => succ := [(nextCodeBlock rest, ref 0)]
                  ;
                  app (addPred blk) (!succ);
                  insertGraphEdges rest
              end
            | insertGraphEdges(_::rest) = insertGraphEdges rest

          (* And entry edges *)
          fun insertEntryEdges() = 
          let val newEntryEdges = 
                  map (fn Label.Label{id, ...} => (lookupLabelMap id,ref 0))
                      (!entryLabels) 
          in  entryEdges := newEntryEdges @ !entryEdges;
              app (addPred entryBlk) newEntryEdges
          end
                      

          val _         = endCurrBlock()
          val allBlocks = rev(!blocks)

             (* clean up *)
          val _         = blocks := []
          val _         = blockNames := []

             (* fill in edges *)
          val _ = fillLabelMap(allBlocks, [])
          val _ = insertGraphEdges(allBlocks)
          val _ = insertEntryEdges() 

             (* create a new cluster *)
          val cluster = 
              F.CLUSTER{blocks=allBlocks, entry=entryBlk, exit=exitBlk,
                        blkCounter=ref(!blkCounter), regmap= !regmap, 
                        annotations=ref(blockAnnotations @ annotations)}

             (* reset regmap *)
          val _         = blkCounter := 0
          val _         = regmap := C.regmap()  
          val _         = aliasF := IntHashTable.insert (!regmap)
          val _         = entryLabels := []
      in  compile cluster
      end

  in  S.STREAM
      {  beginCluster = beginCluster,
         endCluster   = endCluster,
         emit         = emit,
         defineLabel  = defineLabel,
         entryLabel   = entryLabel,
         pseudoOp     = pseudoOp,
         exitBlock    = exitBlock,
         annotation   = annotation,
         comment      = comment,
         alias        = alias,
         phi          = can'tUse
      }
  end

end 
