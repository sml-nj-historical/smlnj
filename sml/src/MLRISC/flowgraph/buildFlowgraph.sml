(* buildFlowgraph.sml
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *)

signature CONTROL_FLOWGRAPH_GEN =
sig

   structure S   : INSTRUCTION_STREAM
   structure I   : INSTRUCTIONS
   structure CFG : CONTROL_FLOW_GRAPH
   		where I = I
		  and P = S.P
   (*
    * This creates an emitter which can be used to build a CFG incrementally
    *)
   type instrStream = 
     (I.instruction, Annotations.annotations, I.C.cellset, CFG.cfg) S.stream

   val build : unit -> instrStream

end

functor BuildFlowgraph 
  (structure Props  : INSN_PROPERTIES
   structure Stream : INSTRUCTION_STREAM
   structure CFG    : CONTROL_FLOW_GRAPH  
     sharing CFG.P = Stream.P
         and CFG.I = Props.I
  ) : CONTROL_FLOWGRAPH_GEN =
struct
  structure CFG = CFG
  structure I = Props.I
  structure P = Props
  structure G = Graph
  structure S = Stream
  structure Fmt = Format
  exception LabelNotFound

  fun dummy (x: CFG.I.C.cellset) = x : Props.I.C.cellset

  type instrStream = 
     (I.instruction, Annotations.annotations, CFG.I.C.cellset, CFG.cfg) S.stream

  fun error msg = MLRiscErrorMsg.error ("BuildFlowGraph", msg)

  val hashLabel = Word.toInt o Label.hash

  fun build ()  = let
    val cfg as ref(G.GRAPH graph) = ref(CFG.new())
   
    (* list of blocks generated so far *)
    val blockList   = ref ([] : CFG.block list)

    (* list of entry labels to patch successors of ENTRY *)
    val entryLabels = ref ([] : Label.label list)
    (* block id associated with a label*)
    val labelMap    = IntHashTable.mkTable(32, LabelNotFound)
    val findLabel   = IntHashTable.find labelMap
    val addLabel    = IntHashTable.insert labelMap

    (* the block name annotation *)
    val blockNames   = ref [] : Annotations.annotations ref

    (* noblock or invalid block has id of ~1 *)
    val noBlock = CFG.newBlock(~1, ref 0)
    (* current block being built up *)
    val currentBlock = ref noBlock

    (* initialize state *)
    fun init () = let
      val G.GRAPH cfg = !cfg
    in
       blockList := [];
       entryLabels := [];
       IntHashTable.clear labelMap;
       blockNames := [];
       currentBlock := noBlock
    end


    (* add a new block and make it the current block being built up *)
    fun newBlock(freq) = let
      val G.GRAPH graph = !cfg
      val id = #new_id graph ()
      val blk as CFG.BLOCK{annotations, ...} = CFG.newBlock(id, ref freq)
    in
      currentBlock := blk;
      annotations := !blockNames;
      blockList := blk :: !blockList;
      #add_node graph (id, blk);
      blk
    end


    (* get current basic block *)
    fun getBlock () = 
     (case !currentBlock of CFG.BLOCK{id= ~1, ...} => newBlock(1) | blk => blk)


    (* ------------------------cluster---------------------------*)
    (* start a new cluster *)
    fun beginCluster _ = init()

    (* emit an instruction *)
    fun emit i = let
      val CFG.BLOCK{insns, ...} = getBlock()
      fun terminate() = currentBlock := noBlock;
    in 
      insns := i:: !insns;
      case Props.instrKind(i)
      of Props.IK_JUMP => terminate()
       | Props.IK_CALL_WITH_CUTS => terminate()
       | _ => ()
      (*esac*)
    end

    (* make current block an exit block *)
    fun exitBlock liveout = let
      fun setLiveOut(CFG.BLOCK{annotations, ...}) = 
	annotations := #create CFG.LIVEOUT liveout :: !annotations
    in 
      case !currentBlock
       of CFG.BLOCK{id= ~1, ...} =>
	   (case !blockList
	     of [] => error "exitBlocks"
	      | blk::_ => setLiveOut blk
	   (*esac*))
        | blk => setLiveOut blk
    end (* exitBlock *)


    (* end cluster --- all done *)
    fun endCluster (annotations) = let
      val cfg as G.GRAPH graph = (!cfg before cfg := CFG.new())
      val _ = CFG.init(cfg)		(* create unique ENTRY/EXIT nodes *)

      val ENTRY = hd(#entries graph ())
      val EXIT = hd(#exits graph ())

      fun addEdge(from, to, kind) =
	#add_edge graph (from, to, CFG.EDGE{k=kind, w=ref 0, a=ref[]})

      fun target lab =
	(case (IntHashTable.find labelMap (hashLabel lab))
	  of SOME bId => bId 
	   | NONE => EXIT)

      fun jump(from, [Props.ESCAPES], _) = addEdge(from, EXIT, CFG.FALLSTHRU)
	| jump(from, [Props.LABELLED lab], _) = addEdge(from, target lab, CFG.JUMP)
	| jump(from, [Props.LABELLED lab, Props.FALLTHROUGH], blks) = let
	   fun next(CFG.BLOCK{id, ...}::_) = id
          in
	    addEdge(from, target lab, CFG.BRANCH true);
	    addEdge(from, next blks, CFG.BRANCH false)
	  end
	| jump(from, [f as Props.FALLTHROUGH, l as Props.LABELLED _], blks) = 
	    jump(from, [l, f], blks)
	| jump(from, targets, _) = let
	    fun switch(Props.LABELLED lab, n) = 
	         (addEdge(from, target lab, CFG.SWITCH(n)); n+1)
	      | switch _ = error "jump.switch"
          in List.foldl switch 0 targets; ()
          end

      and fallsThru(id, blks) = let
	fun fallThruEdge(to) = addEdge (id, to, CFG.FALLSTHRU)
      in
	case blks
	 of [] => fallThruEdge(EXIT)
          | CFG.BLOCK{id=next, insns=ref(_::_), (*data=ref[], JHR *) ...}::_ => fallThruEdge(next)
	  | CFG.BLOCK{id=next, ...} ::_ => error 
	     (* if pseudo ops are alignment directives, this may not be an error *)
	     (Fmt.format "Block %d falls through to pseudoOps in %d\n"
	        [Fmt.INT id, Fmt.INT next])
      end
	     
      and addEdges [] = ()
	| addEdges(CFG.BLOCK{id, insns=ref[], ...}::blocks) = fallsThru(id, blocks)
	| addEdges(CFG.BLOCK{id, insns=ref(instr::_), ...}::blocks) = let
	    fun doJmp () = jump(id, Props.branchTargets instr, blocks)
          in
	   case Props.instrKind instr
	    of Props.IK_JUMP => doJmp()
	     | Props.IK_CALL_WITH_CUTS => doJmp()
	     | _ => fallsThru(id, blocks)
	   (*esac*);
	   addEdges(blocks)
          end
    in
      addEdges (rev(!blockList));
      app (fn lab => addEdge(ENTRY, target lab, CFG.ENTRY)) (!entryLabels);
      let val an = CFG.annotations cfg in  an := annotations @ (!an) end;
      cfg
    end (* endCluster *)

    (* -------------------------labels---------------------------*)
    (* BUG: Does not respect any ordering between labels and pseudoOps. 
     * This could be a problem with jump tables.
     *)
    fun newPseudoOpBlock() =
     (case !currentBlock 
       of CFG.BLOCK{id= ~1,...} => newBlock(1)
	|  b as CFG.BLOCK{insns=ref [],...} => b
	|  _ => newBlock(1)
     (*esac*))

    fun addPseudoOp p = let
      val CFG.BLOCK{data, ...} = newPseudoOpBlock()
    in data := !data @ [CFG.PSEUDO p]
    end

    fun defineLabel lab = (case findLabel (hashLabel lab)
	   of NONE => let
        	val CFG.BLOCK{id, labels, data, ...} = newPseudoOpBlock()
        	in 
		  labels := lab :: !labels;
		  data := !data @ [CFG.LABEL lab];	(* JHR *)
		  addLabel(hashLabel lab, id)
		end
	    | SOME _ => error (concat[
		  "multiple definitions of label \"", Label.toString lab, "\""
		])
	  (* end case *))
      
    fun entryLabel lab = (defineLabel lab; entryLabels := lab :: !entryLabels)



    
    (* ------------------------annotations-----------------------*)
    (* XXX: Bug: EMPTYBLOCK does not really generate an empty block 
     *	but merely terminates the current block. Contradicts the comment
     *  in instructions/mlriscAnnotations.sig.
     *)

    (* Add a new annotation *)
    fun addAnnotation a = 
     (case a 
       of MLRiscAnnotations.BLOCKNAMES names =>
	   (blockNames := names;  newBlock(1); ())
        | MLRiscAnnotations.EMPTYBLOCK => (newBlock(1); ())
	| MLRiscAnnotations.EXECUTIONFREQ f => 
	   (case !currentBlock
	     of CFG.BLOCK{id= ~1, ...} => (newBlock(f); ())
	      | CFG.BLOCK{freq, ...} => freq := f
           (*esac*))
	| a => let 
	     val CFG.BLOCK{annotations,...} = getBlock()
           in  annotations := a :: !annotations
	   end
     (*esac*))

    (* get annotation associated with flow graph *)
    fun getAnnotations () = CFG.annotations(!cfg)

    (* add a comment annotation to the current block *)
    fun comment msg = addAnnotation (#create MLRiscAnnotations.COMMENT msg)
  in
    S.STREAM
      { 
         comment       = comment,
         getAnnotations= getAnnotations,
         annotation    = addAnnotation,
         defineLabel   = defineLabel,
         entryLabel    = entryLabel,
         pseudoOp      = addPseudoOp,
         beginCluster  = beginCluster,
         emit          = emit,
         exitBlock     = exitBlock,
         endCluster    = endCluster
      }
  end
end
