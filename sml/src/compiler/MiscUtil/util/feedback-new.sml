(*
 * feedback[-new].sml
 *  Compute minimum feedback vertex set of a given directed graph.
 *
 *   Copyright (c) 2000 by Lucent Bell Laboratories
 *
 * original version by: Andrew Appel (appel@cs.princeton.edu) (is this right?)
 * 
 * recent cleanup by: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *    The cleanup involves getting rid of duplicate SCC code (using
 *    the library module GraphSCCFn) and making use of integer set-
 *    and map-modules (from the same library).  The use of SortedList
 *    has been eliminated.
 *)
structure Feedback :> sig
    (* Input: A directed graph; that is, a list of vertex-numbers, 
     *        each node with a list of out-edges which indicate other vertices.
     * Output:  A minimum feedback vertex set.
     *
     * Method: branch and bound *)
    type vertex = int
    type node = vertex * vertex list	(* vertex + outgoing edges *)
    type graph = node list
    val feedback : graph -> vertex list
end = struct
    (* NOTE:  By setting MAXDEPTH=infinity, this algorithm will produce
     *        the exact minimum feedback vertex set.  With MAXDEPTH<infinity,
     *        the result will still be a feedback vertex set, but not
     *        always the minimum set.  However, on almost all real programs,
     *        MAXDEPTH=3 will give perfect and efficiently computed results.
     *        Increasing MAXDEPTH will not make the algorithm take longer or
     *        produce better results on "real" programs. *)
    val MAXDEPTH = 3

    type vertex = int
    type node = vertex * vertex list	(* vertex + outgoing edges *)
    type graph = node list

    fun bug s = ErrorMsg.impossible ("Feedback.feedback: " ^ s)

    structure IS = IntRedBlackSet
    fun l2s l = IS.addList (IS.empty, l)
    val s2l = IS.listItems

    structure IM = IntRedBlackMap

    structure Nd = struct
        type ord_key = int
	val compare = Int.compare
    end

    structure SCC = GraphSCCFn (Nd)

    (* "normalize" graph by eliminating edges that lead elsewhere *)
    fun normalize g = let
	val vs = l2s (map #1 g)
	fun prune (v, e) = (v, IS.intersection (e, vs))
    in
	map prune g
    end

    fun scc g = let
	val allbutroot = map #1 g
	val root = (foldl Int.max 0 allbutroot) + 1
	fun add ((v, e), (sm, fm)) =
	    (IM.insert (sm, v, e), IM.insert (fm, v, s2l e))
	val (set_map, follow_map) = foldl add (IM.empty, IM.empty) g
	val follow_map = IM.insert (follow_map, root, allbutroot)
	fun follow v = valOf (IM.find (follow_map, v))
	(* Do the actual scc calculation; for a sanity check we could
	 * match the result against (SCC.SIMPLE root :: _), but we trust
	 * the SCC module and "nontrivial" (below) will take care of
	 * the root node. *)
	val sccres = SCC.topOrder { root = root, follow = follow }
	(* we already eliminate all trivial (= SIMPLE) components here *)
	fun toNode v = (v, valOf (IM.find (set_map, v)))
	fun nontrivial (SCC.SIMPLE _, a) = a
	  | nontrivial (SCC.RECURSIVE l, a) = map toNode l :: a
	val ntcomps = foldr nontrivial [] sccres
    in
	(* we finally make each component "self-contained" by pruning
	 * away all edges that lead out of it... *)
	map normalize ntcomps
    end

    fun feedback graph0 = let
	(* make edges into vertex sets *)
	val graph = map (fn (v, e) => (v, l2s e)) graph0
	(* any node with an edge to itself MUST be in the minimum feedback
	 * vertex set; remove these "selfnodes" first to make the problem
	 * easier. *)
	fun hasSelfLoop (v, e) = IS.member (e, v)
	val (selfnodes, rest) = List.partition hasSelfLoop graph
	(* The following value is part 1 of the final result. *)
	val selfvertices = l2s (map #1 selfnodes)
	(* with missing nodes, the rest needs to be normalized *)
	val rest = normalize rest

	(* here is the branch-and-bound algorithm that is used for the rest *)
	fun feedb (limit, graph, depth) =
	    if depth <= 0 then
		if limit >= length graph then
		    (* approximate! *)
		    SOME (l2s (map #1 graph))
		else
		    (* Note: the original algorithm would have continued
		     * here when depth < 0; but that seems wrong *)
		    NONE
	    else let val comps = scc graph
		     fun g (lim, set, c :: comps) =
			 if lim > 0 then
			     case try (lim, c, depth) of
				 NONE => NONE
			       | SOME vs => g (lim - IS.numItems vs + 1,
					       IS.union (vs, set),
					       comps)
			 else NONE
		       | g (lim, set, []) = SOME set
		 in
		     g (limit - length comps + 1, IS.empty, comps)
		 end

	and try (limit, nodes, depth) = let
	    fun f (best, lim, left, []) = best
	      | f (best, lim, left, (node as (x, e)) :: right) =
		if not (List.null left) andalso IS.numItems e = 1 then
	            (* A node with only one out-edge can't be part of
		     * a unique minimum feedback vertex set, unless they
		     * all have one out-edge. *)
	            f (best, lim, node :: left, right)
		else let
			fun prune (n, es) =
			    (n, IS.delete (es, x)
				handle LibBase.NotFound => es)
			val reduced = map prune (List.revAppend (left, right))
		    in
			case feedb (lim - 1, reduced, depth - 1) of
			    SOME vs => f (SOME (IS.add (vs, x)),
					  IS.numItems vs,
					  node :: left, right)
			  | NONE => f (best, lim, node :: left, right)
		    end
	in
	    f (NONE, Int.min (limit, length nodes), [], nodes)
	end

	fun bab g =
	    case feedb (length g, g, MAXDEPTH) of
		SOME solution => solution
	      | NONE => bug "no solution"
    in
	s2l (IS.union (selfvertices, bab rest))
    end
end
