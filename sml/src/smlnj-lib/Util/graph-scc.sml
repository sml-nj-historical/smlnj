(* graph-scc.sml
 *
 * COPYRIGHT (c) 1999 Lucent Bell Laboratories.
 *
 *   Calculate strongly-connected components of directed graph.
 *   The graph can have nodes with self-loops.
 *
 * author: Matthias Blume
 *) 

functor GraphSCCFn (Nd: ORD_KEY) :> GRAPH_SCC where Nd = Nd =
  struct

    structure Nd = Nd

    type node = Nd.ord_key

    structure Map = RedBlackMapFn (Nd)

    datatype component
      = SIMPLE of node
      | RECURSIVE of node list

    fun eq x y = (Nd.compare(x, y) = EQUAL)

    fun topOrder { root, follow } = let

	fun getNode (n, nm as (npre, m)) = (
	      case Map.find (m, n)
	       of NONE => let
		    val r = { pre = npre, low = ref npre }
		    val m' = Map.insert (m, n, r)
		    in
		      ((npre + 1, m'), r)
		    end
		| SOME r => (nm, r)
	      (* end case *))

	fun component (x, []) =
	    if List.exists (eq x) (follow x) then RECURSIVE [x]
	    else SIMPLE x
	  | component (x, xl) = RECURSIVE (x :: xl)

	(* depth-first search in continuation-passing, state-passing style *)
	fun dfs args = let

	    (* the nodemap represents the mapping from nodes to
	     * pre-order numbers and low-numbers. The latter are ref-cells.
	     * nodemap also remembers the next available pre-order number. *)
	    val { node, node_pre, node_low, parent_low, nodemap,
		  stack, sccl, cont } = args

	    (* loop over the follow-set of a node *)
	    fun loop (tn :: tnl) (nodemap as (npre, theMap), stack, sccl) = 
		(case Map.find (theMap, tn) of
		     SOME{ pre = tn_pre, low = tn_low } => let
			 val tl = !tn_low
		     in
			 if tl < (!node_low) andalso
			     List.exists (eq tn) stack then
			     node_low := tl
			 else ();
			 loop tnl (nodemap, stack, sccl)
                     end
                  | NONE =>let
			(* lookup failed -> tn is a new node *)
			val tn_pre = npre
			val tn_low = ref npre
			val npre = npre + 1
			val theMap = 
			    Map.insert (theMap, tn,
					{ pre = tn_pre, low = tn_low })
			val nodemap = (npre, theMap)
		    in
			dfs { node = tn, node_pre = tn_pre, node_low = tn_low,
			      parent_low = node_low,
			      nodemap = nodemap,
			      stack = tn :: stack,
			      sccl = sccl,
			      cont = loop tnl }
		    end)
	      | loop [] (nodemap, stack, sccl) = let
		    val nl = !node_low
		in
		    if nl = node_pre then let
			fun grab (top :: stack, scc) =
			    if eq top node then
				cont (nodemap, stack,
				      component (top, scc) :: sccl)
			    else grab (stack, top :: scc)
			  | grab _ = raise Fail "scc.sml: grab: empty stack"
		    in
			grab (stack, [])
		    end
		    else
			((* propagate node_low up *)
			 if nl < (!parent_low) then parent_low := nl else ();
			 (* `return' *)
			 cont (nodemap, stack, sccl))
		end
	in
	    loop (follow node) (nodemap, stack, sccl)
	end

	val root_low = ref 0
    in
	dfs { node = root, node_pre = 0, node_low = root_low,
	      parent_low = ref 0,	(* dummy *)
	      nodemap = (1, Map.insert (Map.empty, root,
					{ pre = 0, low = root_low })),
	      stack = [root],
	      sccl = [],
	      cont = fn (_, _, sccl) => sccl }
    end

  end
