(* weighted-block-placement-fn.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *
 * This functor implements the bottom-up block-placement algorithm of
 * Pettis and Hansen (PLDI 1990).
 *
 * TODO
 *	remove low-weight nodes to break cycles in chain graph
 *)

functor WeightedBlockPlacementFn (

    structure CFG : CONTROL_FLOW_GRAPH
    structure InsnProps : INSN_PROPERTIES
      where I = CFG.I

  ) : BLOCK_PLACEMENT = struct

    structure CFG = CFG
    structure IP = InsnProps
    structure G = Graph
    structure ITbl = IntHashTable
    structure PQ = LeftPriorityQFn (
      struct
	type priority = CFG.weight
	val compare = Real.compare
	type item = CFG.edge
	fun priority (_, _, CFG.EDGE{w, ...}) = !w
      end)

  (* flags *)
    val dumpBlocks = MLRiscControl.getFlag "dump-block-list"
    val dumpCFG = MLRiscControl.getFlag "dump-cfg-after-placement"
    val dumpStrm = MLRiscControl.debug_stream

  (* sequences with constant-time concatenation *)
    datatype 'a seq
      = ONE of 'a
      | SEQ of ('a seq * 'a seq)

  (* a chain of blocks that should be placed in order *)
    datatype chain = CHAIN of {
	blocks : CFG.node seq,
	hd : CFG.node,
	tl : CFG.node
      }

    fun head (CHAIN{hd, ...}) = #1 hd
    fun tail (CHAIN{tl, ...}) = #1 tl
    fun id (CHAIN{hd, ...}) = #1 hd	(* use node ID of head to identify chains *)
    fun sameChain (CHAIN{hd=h1, ...}, CHAIN{hd=h2, ...}) = (#1 h1 = #1 h2)

    fun blockToString (id', CFG.BLOCK{id, ...}) =
	  concat["<", Int.toString id', ":", Int.toString id, ">"]

    fun chainToString (CHAIN{hd, blocks, ...}) = let
	  fun seq (ONE blk, l) = blockToString blk :: l
	    | seq (SEQ(s1, s2), l) = seq(s1, "," :: seq(s2, l))
	  in
	    concat("CHAIN{" :: blockToString hd :: ",[" :: seq(blocks, ["]}"]))
	  end

  (* join two chains *)
    fun joinChains (CHAIN{blocks=b1, hd, ...}, CHAIN{blocks=b2, tl, ...}) =
	  CHAIN{blocks=SEQ(b1, b2), hd=hd, tl=tl}

    val unifyChainPtrs = URef.unify joinChains

  (* chain pointers provide a union-find structure for chains *)
    type chain_ptr = chain URef.uref

    type block_chain_tbl = chain_ptr ITbl.hash_table

  (* a directed graph representing the placement ordering on chains. An edge
   * from chain c1 to c2 means that we should place c1 before c2.  The graph
   * may be cyclic, so we weight the edges and remove the low-cost edge
   * on any cycle.
   *)
    datatype node = ND of {
	chain : chain,
	mark : bool ref,
	kids : edge list ref
      }
    and edge = E of {
	w : CFG.weight,
	dst : node,
	ign : bool ref		(* if set, then ignore this edge.  We use this *)
				(* flag to break cycles. *)
      }

    fun mkNode c = ND{chain = c, mark = ref false, kids = ref []}
    fun mkEdge (w, dst) = E{w = w, dst = dst, ign = ref false}

  (* given a table that maps block IDs to chain pointers, construct a table that
   * maps block IDs to their chain-placement graph nodes.
   *)
    fun mkChainPlacementGraph (tbl : block_chain_tbl) = let
	  val gTbl = ITbl.mkTable (ITbl.numItems tbl, Fail "graph table")
	  val find = ITbl.find gTbl
	  val insert = ITbl.insert gTbl
	(* given a block ID and the chain pointer corresponding to the block
	 * add the chain node to the graph table (this may involve creating
	 * the node if it doesn't already exist).
	 *)
	  fun blockToNd (blkId, cptr, nodes) = let
		val chain = URef.!! cptr
		val chainId = id chain
		in
		  case find chainId
		   of NONE => let
			val nd = mkNode chain
			in
			  insert (chainId, nd);
			  if (blkId <> chainId)
			    then insert (blkId, nd)
			    else ();
			  nd :: nodes
			end
		    | SOME nd => (insert (blkId, nd); nodes)
		  (* end case *)
		end
	  in
	    (ITbl.foldi blockToNd [] tbl, gTbl)
	  end

    fun blockPlacement (cfg as G.GRAPH graph) = let
	(* a map from block IDs to their chain *)
	  val blkTbl : chain_ptr ITbl.hash_table = let
		val tbl = ITbl.mkTable (#size graph (), Fail "blkTbl")
		val insert = ITbl.insert tbl
		fun ins (b : CFG.node) = insert (#1 b,
		      URef.uRef(CHAIN{blocks = ONE b, hd = b, tl = b}))
		in
		  #forall_nodes graph ins;
		  tbl
		end
	  val lookupChain = ITbl.lookup blkTbl
	(* the unique exit node *)
	  val exitId = hd(#exits graph ())
	(* given an edge that connects two blocks, attempt to merge their chains.
	 * We return true if a merge occurred.  We do not join exit edges so that
	 * the exit and entry nodes end up in distinct chains.
	 *)
	  fun join (src, dst, _) = if (dst = exitId)
		then false
		else let
		  val cptr1 = lookupChain src
		  val chain1 = URef.!! cptr1
		  val cptr2 = lookupChain dst
		  val chain2 = URef.!! cptr2
		  in
		    if (tail chain1 = src) andalso (dst = head chain2)
		    andalso not(sameChain(chain1, chain2))
		      then (
		      (* the source block is the tail of its chain and the
		       * destination block is the head of its chain, so we can
		       * join the chains.
		       *)
			ignore (unifyChainPtrs (cptr1, cptr2));
			true)
		      else false (* we cannot join these chains *)
		  end
	(* merge chains until all of the edges have been examined; the remaining
	 * edges cannot be fall-through.
	 *)
	  fun loop (pq, edges) = (case PQ.next pq
		 of SOME(edge, pq) => if join edge
		      then loop (pq, edges)
		      else loop (pq, edge::edges)
		  | NONE => edges
		(* end case *))
	  val edges = loop (PQ.fromList (#edges graph ()), [])
	(* construct a chain placement graph *)
	  val (chainNodes, grTbl) = mkChainPlacementGraph blkTbl
	  val lookupNd = ITbl.lookup grTbl
	  fun addCFGEdge (src, dst, CFG.EDGE{k, w, ...}) = (case k
(* NOTE: there may be icache benefits to including SWITCH edges. *)
		 of CFG.SWITCH _ => ()
		  | CFG.FLOWSTO => ()
		  | _ => let
		      val ND{chain=c1, kids, ...} = lookupNd src
		      val dstNd as ND{chain=c2, ...} = lookupNd dst
		      in
			if sameChain(c1, c2)
			  then ()
			  else kids := mkEdge (!w, dstNd) :: !kids
		      end
		(* end case *))
	  val _ = List.app addCFGEdge edges
(* FIXME: we should remove low-weight nodes to break cycles *)
	(* now we construct an ordering on the chains by doing a DFS on the
	 * chain graph.
	 *)
	  fun dfs (ND{mark = ref true, ...}, l) = l
	    | dfs (ND{mark, chain, kids, ...}, l) = let
		fun addKid (E{ign=ref true, ...}, l) = l
		  | addKid (E{dst, ...}, l) = dfs (dst, l)
		in
		  mark := true;
		  List.foldl addKid (chain::l) (!kids)
		end
	(* mark the exit node, since it should be last.  Note that we
	 * ensured above that the exit and entry nodes are in distinct
	 * chains!
	 *)
	  val exitChain = let
		val ND{chain, mark, ...} = lookupNd(hd(#exits graph ()))
		in
		  mark := true;
		  chain
		end
	(* start with the entry node *)
	  val chains = dfs (lookupNd(hd(#entries graph ())), [])
	(* place the rest of the nodes and add the exit node *)
	  val chains = List.foldl dfs chains chainNodes
	  val chains = exitChain :: chains
	(* extract the list of blocks from the chains list; the chains list is
	 * in reverse order.  The resulting list of blocks is in order.
	 *)
	  fun addChain (CHAIN{blocks, ...}, blks) = let
		fun addSeq (ONE b, blks) = b::blks
		  | addSeq (SEQ(s1, s2), blks) = addSeq(s1, addSeq(s2, blks))
		in
		  addSeq (blocks, blks)
		end
	  val blocks = List.foldl addChain [] chains
	  fun updEdge (CFG.EDGE{w, a, ...}, k) = CFG.EDGE{w=w, a=a, k=k}
	  fun flipJmp (insns as ref(i::r), lab) =
		insns := IP.negateConditional(i, lab) :: r
	  val setEdges = #set_out_edges graph
	(* map a block ID to a label *)
	  fun labelOf blkId = (case #node_info graph blkId
		 of CFG.BLOCK{labels=ref(lab::_), ...} => lab
		  | CFG.BLOCK{labels, ...} => let
		      val lab = Label.anon()
		      in
			labels := [lab];
			lab
		      end
		(* end case *))
	(* patch the blocks so that unconditional jumps to the immediate successor
	 * are replaced by fall-through edges and conditional jumps to the immediate
	 * successor are negated.  Remember that we cannot fall through to the exit
	 * block!
	 *)
	  fun patch (
		(blkId, CFG.BLOCK{kind=CFG.NORMAL, insns, ...}),
		(next as (nextId, _)) :: rest
	      ) = (
		case #out_edges graph blkId
		 of [(_, dst, e as CFG.EDGE{k, w, a})] => (case (dst = nextId, k)
		       of (false, CFG.FALLSTHRU) => (
			  (* rewrite edge as JUMP and add jump insn *)
			    setEdges (blkId, [(blkId, dst, updEdge(e, CFG.JUMP))]);
			    insns := IP.jump(labelOf dst) :: !insns)
			| (true, CFG.JUMP) =>
			    if (nextId <> exitId)
			      then (
			      (* rewrite edge as FALLSTHRU and remove jump insn *)
				setEdges (blkId,
				  [(blkId, dst, updEdge(e, CFG.FALLSTHRU))]);
				insns := tl(!insns))
			      else () (* do not rewrite jumps to STOP block *)
			| _ => ()
		      (* end case *))
		  | [(_, dst1, e1 as CFG.EDGE{k=CFG.BRANCH b, ...}),
		      (_, dst2, e2)
		    ] => (case (dst1 = nextId, b)
		       of (true, true) => (
			    setEdges (blkId, [
				(blkId, dst1, updEdge(e1, CFG.BRANCH false)),
				(blkId, dst2, updEdge(e2, CFG.BRANCH true))
			      ]);
			    flipJmp (insns, labelOf dst2))
			| (false, false) => (
			    setEdges (blkId, [
				(blkId, dst1, updEdge(e1, CFG.BRANCH true)),
				(blkId, dst2, updEdge(e2, CFG.BRANCH false))
			      ]);
			    flipJmp (insns, labelOf dst1))
			| _ => ()
		      (* end case *))
		  | _ => ()
		(* end case *);
		patch (next, rest))
	    | patch (_, next::rest) = patch(next, rest)
	    | patch (_, []) = ()
	  in
	    patch (hd blocks, tl blocks);
	    if !dumpBlocks
	      then let
		fun say s = TextIO.output(!dumpStrm, s)
		in
		  say "Block placement order:\n";
		  List.app
		    (fn b => say(concat["  ", blockToString b, "\n"]))
		      blocks
		end
	      else ();
	    if !dumpCFG
	      then let
		val prBlock = CFG.dumpBlock (!dumpStrm, cfg)
		in
		  TextIO.output(!dumpStrm, "[ after block placement ]\n");
		  List.app prBlock blocks
		end
	      else ();
	    blocks
	  end

  end
