(* flowgen.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

functor ClusterGen
  (structure Flowgraph : FLOWGRAPH
   structure InsnProps : INSN_PROPERTIES
   structure MLTree : MLTREE

   val output : Flowgraph.cluster -> unit
     sharing Flowgraph.I = InsnProps.I
     sharing MLTree.Constant = InsnProps.I.Constant  
     sharing MLTree.PseudoOp = Flowgraph.P 
     sharing Flowgraph.B = MLTree.BNames 
  ) : FLOWGRAPH_GEN = 
struct

  structure F = Flowgraph
  structure Props = InsnProps
  structure I = Flowgraph.I
  structure C = I.C

  structure T = MLTree
  structure B = MLTree.BNames
  structure P = T.PseudoOp
  structure S = T.Stream
  
  type label = Label.label

  fun error msg = MLRiscErrorMsg.error("ClusterGen",msg)

  type flowgraph = F.cluster

  fun newStream() = 
  let val bblkCnt = ref 0 
      val entryLabels = ref ([] : Label.label list)
      val blkName  = ref B.default 
      val regmap  = ref NONE
      fun can'tUse _ = error "unimplemented" 
      val aliasF  = ref can'tUse : (T.alias -> unit) ref
      val NOBLOCK = F.EXIT{blknum=0,freq=ref 0,pred=ref []}
      val currBlock : F.block ref = ref NOBLOCK
      val blockList : F.block list ref = ref []
    
      fun nextBlkNum () = !bblkCnt before bblkCnt := !bblkCnt + 1
      fun blockName name = 
        (case !currBlock
         of blk as F.BBLOCK _ => 
             (currBlock := NOBLOCK; blockList := blk:: !blockList)
          | _ => ()
         (*esac*);
         blkName := name)
    
      (** Note - currBlock will always be a reference to a F.BBLOCK{..} **)
      fun newBasicBlk init = 
          F.BBLOCK{blknum=nextBlkNum(),
                   freq=ref 0,
                   annotations=ref [],
                   name= !blkName,
                   liveIn=ref C.empty,
                   liveOut=ref C.empty,
                   succ=ref [],
                   pred=ref [],
                   insns=ref init}
      local
        fun blockListAdd b = let
          val blocks = !blockList
        in
          case !currBlock 
           of blk as F.BBLOCK _ => (blockList:=b::blk::blocks; 
                                    currBlock:=NOBLOCK)
            | _ => blockList := b::blocks
        end
      in
        fun pseudoOp pOp  = blockListAdd (F.PSEUDO pOp)
        fun defineLabel lab = blockListAdd(F.LABEL lab)
        fun entryLabel lab = 
          (entryLabels := lab::(!entryLabels);  blockListAdd(F.LABEL lab))
      end (*local*)
    
      (** emitInstr - instructions are always added to currBlock. **)
      fun emitInstr instr = let
         fun addInstr (F.BBLOCK{insns, ...}) = insns := instr::(!insns)
           | addInstr _ = currBlock:=newBasicBlk [instr]
      in addInstr(!currBlock);
         case Props.instrKind instr
          of Props.IK_JUMP => 
              (blockList:= !currBlock :: (!blockList);
               currBlock := NOBLOCK)
           | _ => ()
         (*esac*)
      end      
      fun annotation a = 
         case !currBlock of
           F.BBLOCK{annotations,...} => annotations := a :: !annotations
         | _ => (currBlock := newBasicBlk []; annotation a)
    
      fun exitBlock liveRegs  = let
        val addReg   = C.addCell C.GP
        val addFreg  = C.addCell C.FP
        val addCCreg = C.addCell C.CC
        (* we don't care about memory locations that may be live. *)
        fun live(T.GPR(T.REG(_,r))::rest, acc) = live(rest, addReg(r, acc))
          | live(T.FPR(T.FREG(_,f))::rest, acc) = live(rest, addFreg(f, acc))
          | live(T.CCR(T.CC c)::rest, acc) = live(rest, addCCreg(c, acc))
          | live(_::rest, acc) = live(rest, acc)
          | live([], acc) = acc
    
        val lout = live(liveRegs, C.empty)
    
        fun findCodeBlock(F.BBLOCK{liveOut,...}::_)  = liveOut
          | findCodeBlock(F.LABEL _::blks) = findCodeBlock blks
          | findCodeBlock _                = error "exitBlock.codeBlock"
    
      in
        case !currBlock
         of F.BBLOCK{liveOut, ...} =>
            (liveOut := lout;
             blockList := !currBlock :: (!blockList);
             currBlock := NOBLOCK)
          | _ => 
            let val outRef = findCodeBlock (!blockList)
            in  outRef := lout
            end
       (*esac*)
      end

      fun endCluster(annotations) = let
          exception LabTbl
          val labTbl : F.block Intmap.intmap = Intmap.new(16, LabTbl)
          val addLabTbl = Intmap.add labTbl
          val lookupLabTbl = Intmap.map labTbl
    
          (* find next code block *)
          exception NextCodeBlock
          fun nextCodeBlock((blk as F.BBLOCK _)::_) = blk
            | nextCodeBlock(_::rest) = nextCodeBlock rest
            | nextCodeBlock [] = raise NextCodeBlock
    
          (* mapping of labels to code blocks *)
          fun fillLabTbl(F.LABEL lab::blks) = 
                (addLabTbl(Label.id lab, nextCodeBlock blks) 
                                            handle NextCodeBlock => ();
                 fillLabTbl blks)
            (*| fillLabTbl(F.ORDERED labs::blks) = fillLabTbl(labs@blks)*)
            | fillLabTbl(_::blks) = fillLabTbl(blks)
            | fillLabTbl [] = ()
    
          val exitBlk = F.EXIT{blknum=nextBlkNum(), pred=ref [], freq=ref 0}
    
          (** update successor and predecessor information **)
          fun graphEdges((blk as F.BBLOCK{blknum,insns,succ,...})::blks) = let
                fun updtPred(F.BBLOCK{pred, ...},w) = pred := (blk,w)::(!pred)
                  | updtPred(F.EXIT{pred, ...},w) = pred := (blk,w)::(!pred)
    
                fun succBlks([], acc) = acc
                  | succBlks(Props.FALLTHROUGH::labs, acc) =
                      ((succBlks(labs, (nextCodeBlock blks,ref 0)::acc))
                        handle NextCodeBlock => error  "graphEdges.succBlks")
                  | succBlks(Props.LABELLED lab::labs, acc) =
                      ((succBlks(labs,(lookupLabTbl(Label.id lab),ref 0)::acc))
                        handle LabTbl => 
                          succBlks(labs, (exitBlk,ref 0)::acc))
                  | succBlks(Props.ESCAPES::labs,acc) = 
                       succBlks(labs, (exitBlk,ref 0)::acc)
    
                val lastInstr = ((hd (!insns))
                         handle _ => error "endCluster.graphEdges.lastInstr")
    
                fun lastCodeBlock(F.BBLOCK _ :: _) = false
                  | lastCodeBlock(_::rest) = lastCodeBlock rest
                  | lastCodeBlock [] = true
              in
                case Props.instrKind lastInstr
                 of Props.IK_JUMP => succ:=succBlks 
                                    (Props.branchTargets lastInstr,[])
                  | _  => 
                    if lastCodeBlock blks then
                      succ := [(exitBlk,ref 0)] 
                                (* control must escape via trap *)
                    else succ := [(nextCodeBlock blks,ref 0)] 
                (*esac*);
                app updtPred (!succ);
                graphEdges(blks)
              end
             | graphEdges(_::blks) = graphEdges(blks)
            | graphEdges [] = ()
    
          fun mkEntryBlock () = let
            val blocks = 
                 map (fn Label.Label{id,...} => (lookupLabTbl id,ref 0)) 
                       (!entryLabels)
            val entryBlk = F.ENTRY{blknum=nextBlkNum(), freq=ref 0,
                                   succ=ref blocks}
          in
            app (fn (F.BBLOCK{pred, ...},w) => 
                  pred := (entryBlk,w)::(!pred)) blocks;
            entryBlk
          end
    
          val _ = case !currBlock
            of blk as F.BBLOCK _ => blockList := blk :: !blockList
             | _ => ()
    
          val blocks = rev(!blockList) 
          val _ = blockList := []
          val _ = fillLabTbl(blocks)
          val _ = graphEdges(blocks)
          val cluster =
           F.CLUSTER{blocks=blocks, entry=mkEntryBlock(), exit=exitBlk,
                     blkCounter=ref(!bblkCnt), regmap= Option.valOf(!regmap),
                     annotations=ref(annotations)}
          val _ = regmap := NONE
          val _ = aliasF := can'tUse
        in  output cluster
        end
    
      fun beginCluster _ = 
      let val map = C.regmap()
      in  entryLabels := [];
          bblkCnt := 0;
          blkName := B.default;
          blockList := [];
          currBlock := NOBLOCK;
          regmap := SOME map;
          aliasF := Intmap.add map;
          map
      end 

      fun comment msg = annotation(BasicAnnotations.COMMENT msg)
      fun alias(v,r) = !aliasF(v,r)

   in S.STREAM
      { beginCluster= beginCluster,
        endCluster  = endCluster,
        emit        = emitInstr,
        defineLabel = defineLabel,
        entryLabel  = entryLabel,
        pseudoOp    = pseudoOp,
        exitBlock   = exitBlock,
        blockName   = blockName,
        annotation  = annotation,
        comment     = comment,
        alias       = alias,
        phi         = can'tUse
      }
   end
  
end

