(* scc.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 * Calculate strongly-connected components of directed graph.
 * The graph can have nodes with self-loops.
 *
 * author: Matthias Blume
 *
 *) 

signature SCCNODE = sig
    type node
    val eq: node * node -> bool
    val lt: node * node -> bool
end

signature SCC = sig

    structure Node: SCCNODE

    datatype component =
	SIMPLE of Node.node		(* singleton, no self-loop *)
      | RECURSIVE of Node.node list

    (* take root node and follow function and return
     * list of topologically sorted strongly-connected components;
     * root component goes first *)
    val topOrder:
	{ root: Node.node, follow: Node.node -> Node.node list } ->
	component list
end

functor SCCUtilFun (structure Node: SCCNODE): SCC = struct

    exception SccBug

    structure Node = Node

    structure NodeOrdSet = struct
	type elem = Node.node
	val (op <) = Node.lt
    end

    structure Map = MapF (NodeOrdSet)

    type node = Node.node

    datatype component =
	SIMPLE of node
      | RECURSIVE of node list

    fun topOrder { root, follow } = let

	fun getNode (n, nm as (npre, m)) =
	    (nm, (Map.lookup m n))
	    handle Map.MapF => let
		val r = { pre = npre, low = ref npre }
		val m' = Map.add (m, n, r)
	    in
		((npre + 1, m'), r)
	    end

	fun theNode x y = Node.eq (x, y)

	fun component (x, []) =
	    if List.exists (theNode x) (follow x) then
		RECURSIVE [x]
	    else
		SIMPLE x
	  | component (x, xl) = RECURSIVE (x :: xl)

	(* depth-first search in continuation-passing, state-passing style *)
	fun dfs args = let

	    (* the nodemap represents the mapping from nodes to
	     * pre-order numbers and low-numbers. The latter are ref-cells.
	     * nodemap also remembers the next available pre-order number. *)
	    val { node, node_pre, node_low, parent_low, nodemap,
		  stack, sccl, cont } = args

	    (* loop over the follow-set of a node *)
	    fun loop [] (nodemap, stack, sccl) =
		let
		    val nl = !node_low
		in
		    if nl = node_pre then
			let
			    fun grab (top :: stack, scc) =
				if Node.eq (top, node) then
				    cont (nodemap, stack,
					  component (top, scc) :: sccl)
				else
				    grab (stack, top :: scc)
			      | grab _ = raise SccBug
			in
			    grab (stack, [])
			end
		    else let
			val _ = 
			    (* propagate node_low up *)
			    if nl < (!parent_low) then parent_low := nl else ()
		    in
			(* `return' *)
			cont (nodemap, stack, sccl)
		    end
		end
	      | loop (tn :: tnl) (nodemap as (npre, theMap), stack, sccl) = let
		    val { pre = tn_pre, low = tn_low } = Map.lookup theMap tn
		    (* the lookup succeeded -> we have seen tn before *)
		    val tl = !tn_low
		in
		    if tl < (!node_low) andalso
		       List.exists (theNode tn) stack then
			node_low := tl
		    else ();
			loop tnl (nodemap, stack, sccl)
		end handle Map.MapF => let
		    (* lookup failed -> tn is a new node *)
		    val tn_pre = npre
		    val tn_low = ref npre
		    val npre = npre + 1
		    val theMap = Map.add (theMap, tn,
					  { pre = tn_pre, low = tn_low })
		    val nodemap = (npre, theMap)
		in
		    dfs { node = tn, node_pre = tn_pre, node_low = tn_low,
			  parent_low = node_low,
			  nodemap = nodemap,
			  stack = tn :: stack,
			  sccl = sccl,
			  cont = loop tnl }
		end
	in
	    loop (follow node) (nodemap, stack, sccl)
	end

	val root_low = ref 0
    in
	dfs { node = root, node_pre = 0, node_low = root_low,
	      parent_low = ref 0,	(* dummy *)
	      nodemap = (1, Map.singleton (root, { pre = 0, low = root_low })),
	      stack = [root],
	      sccl = [],
	      cont = fn (_, _, sccl) => sccl }
    end
end

(*
 * $Log: scc.sml,v $
 * Revision 1.1.1.1  1997/01/14  01:38:49  george
 *   Version 109.24
 *
 *)
