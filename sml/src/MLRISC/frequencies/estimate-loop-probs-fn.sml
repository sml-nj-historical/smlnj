(* estimate-loop-probs-fn.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies.
 *
 * Given a CFG that may have some existing edge probabilities
 * (represented as BRANCHPROB annotations) add probabilities
 * based on the loop structure using the heuristics from Ball-Larus
 * and Wu-Larus.
 *)

functor EstimateLoopProbsFn (
    structure CFG : CONTROL_FLOW_GRAPH
  ) : sig

    structure CFG : CONTROL_FLOW_GRAPH

    val estimate : CFG.cfg -> unit

  end = struct

    structure CFG = CFG
    structure Dom = DominatorTree(DirectedGraph)
    structure LP = LoopStructure(
      structure GraphImpl = DirectedGraph
      structure Dom = Dom)
    structure An = Annotations
    structure Prob = Probability

    local
      structure A = MLRiscAnnotations
      val {get, set, ...} = A.BRANCH_PROB
    in
    fun getEdgeProb (_, _, CFG.EDGE{a, ...}) = get(!a)
    fun setEdgeProb ((_, _, CFG.EDGE{a, ...}), p) = a := set(p, !a)
    end

  (* the "Loop branch heuristic" *)
    val probLBH = Prob.percent 88
    val prob50_50 = 50

  (* Compute loop structure information *)
    fun computeLoopStructure cfg = let
	  val domTree = Dom.makeDominator cfg
	  val dominates = Dom.dominates domTree
	  val Graph.GRAPH{has_node, ...} = LP.loop_structure domTree
	  in
	    { isLoopHeader = has_node,
	      isBackEdge = 
		fn (src, dst, _) => has_node dst andalso dominates(dst, src)
	    }
	  end

  (* add loop probabilities *)
    fun estimate (cfg as Graph.GRAPH{forall_nodes, out_edges, ...}) = let
	  val {isLoopHeader, isBackEdge} = computeLoopStructure cfg
	  fun edgeProb [e1, e2] = let
		fun computeProbs (trueE, falseE, takenProb) = (
		      case (getEdgeProb trueE, getEdgeProb falseE)
		       of (NONE, NONE) =>
			    {t=takenProb, f=Prob.-(Prob.always, takenProb)}
			| (SOME p, _) =>
			    Prob.combineProb2 {trueProb=p, takenProb=takenProb}
			| (_, SOME p) => 
			    Prob.combineProb2 {
				trueProb=Prob.-(Prob.always, p),
				takenProb=takenProb
			      }
		      (* end case *))
		in
		  case (isBackEdge e1, isBackEdge e2)
		   of (true, false) => let
			val {t, f} = computeProbs(e1, e2, probLBH)
			in
			  setEdgeProb(e1, t);
			  setEdgeProb(e2, f)
			end
		    | (false, true) => let
			val {t, f} = computeProbs(e2, e1, probLBH)
			in
			  setEdgeProb(e2, t);
			  setEdgeProb(e1, f)
			end
		    | _ => ()
		  (* end case *)
		end
	    | edgeProb _ = ()
	  fun doBlock (id, CFG.BLOCK{kind=CFG.NORMAL, ...}) =
		edgeProb (out_edges id)
	    | doBlock _ = ()
	  in
	    forall_nodes doBlock
	  end

  end
