(*
 * Get the set of reachable SNODEs in a given dependency graph.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature REACHABLE = sig
    (* These two functions simply give you the set of (non-stable)
     * modules reachable from some root and the fringe of stable
     * modules that surrounds the non-stable portion. *)
    val reachable' :
	DependencyGraph.sbnode list -> SmlInfoSet.set * StableSet.set
    val reachable : GroupGraph.group -> SmlInfoSet.set * StableSet.set

    (* "snodeMap" gives us handles at arbitrary points within the (non-stable)
     * portion of a dependency graph.
     * This is used by "slave" mode compiler. *)
    val snodeMap : GroupGraph.group -> DependencyGraph.snode SrcPathMap.map

    (* Given a library (or group) g, "groupsOf g" gets the set of
     * subgroups (but not sub-libraries) of that group.  The result
     * will include the argument itself. *)
    val groupsOf : GroupGraph.group -> SrcPathSet.set

    (* Given an arbitrary group graph rooted at group g, "stableLibsOf g"
     * gets the set of stable libraries reachable from g. *)
    val stableLibsOf : GroupGraph.group -> GroupGraph.group SrcPathMap.map

    (* Given a "closed" subset of (non-stable) nodes in a dependency graph,
     * "frontier" gives you the set of frontier nodes of that set.  The
     * closed set is given by its indicator function (first argument).
     * ("closed" means that if a node's ancestors are all in
     * the set, then so is the node itself.  A frontier node is a node that
     * is in the set but either not all of its ancestors are or the node
     * is an export node.) *)
    val frontier : (BinInfo.info -> bool) -> GroupGraph.group -> StableSet.set
end

structure Reachable :> REACHABLE = struct
    structure DG = DependencyGraph
    structure GG = GroupGraph

    local
	fun reach ops (export_nodes: DG.sbnode list) = let
	    val { add, member, empty } = ops
	    fun snode (x as DG.SNODE n, (known, stabfringe)) = let
		val { smlinfo = i, localimports = l, globalimports = g } = n
	    in
		if member (known, i) then (known, stabfringe)
		else foldl farsbnode
		           (foldl snode (add (known, i, x), stabfringe) l)
			   g
	    end
	
	    and farsbnode ((_, n), ksf) = sbnode (n, ksf)
		
	    and sbnode (DG.SB_BNODE (DG.BNODE n, _), (known, stabfringe)) =
		(known, StableSet.add (stabfringe, #bininfo n))
	      | sbnode (DG.SB_SNODE n, ksf) = snode (n, ksf)
	in
	    foldl sbnode (empty, StableSet.empty) export_nodes
	end

	fun snodeMap' (exports: DG.impexp SymbolMap.map, acc) = let
	    fun add (m, i, x) = SrcPathMap.insert (m, SmlInfo.sourcepath i, x)
	    fun member (m, i) = SrcPathMap.inDomain (m, SmlInfo.sourcepath i)
	in
	    #1 (reach { add = add, member = member, empty = acc }
		      (map (#2 o #1) (SymbolMap.listItems exports)))
	end
    in
	val reachable' =
	    reach { add = fn (s, i, _) => SmlInfoSet.add (s, i),
		    member = SmlInfoSet.member,
		    empty = SmlInfoSet.empty }

	fun reachable (GG.GROUP { exports, ... }) =
	              reachable' (map (#2 o #1) (SymbolMap.listItems exports))

	fun snodeMap g = let
	    fun snm (g, (a, seen)) = let
		val GG.GROUP { exports, sublibs, grouppath, ... } = g
	    in
		if SrcPathSet.member (seen, grouppath) then (a, seen)
		else foldl (fn ((_, g), x) => snm (g, x))
		           (snodeMap' (exports, a),
			    SrcPathSet.add (seen, grouppath))
			   sublibs
	    end
	in
	    #1 (snm (g, (SrcPathMap.empty, SrcPathSet.empty)))
	end

	fun groupsOf g = let
	    fun subgroups (GG.GROUP { kind = GG.NOLIB sg, ... }) = sg
	      | subgroups (GG.GROUP { kind = GG.LIB (_, sg), ... }) = sg
	      | subgroups _ = []
	    fun go (g as GG.GROUP { grouppath, ... }, a) = let
		val sgl = subgroups g
		fun sl ((p, g as GG.GROUP { kind = GG.NOLIB _, ... }), a) =
		    if SrcPathSet.member (a, p) then a else go (g, a)
		  | sl (_, a) = a
	    in
		SrcPathSet.add (foldl sl a sgl, grouppath)
	    end
	in
	    go (g, SrcPathSet.empty)
	end

	fun stableLibsOf (g as GG.GROUP { grouppath, ... }) = let
	    fun slo ((p, g), (seen, res)) = let
		val GG.GROUP { kind, sublibs, ... } = g
	    in
		if SrcPathSet.member (seen, p) then (seen, res)
		else let
		    val (seen, res) = foldl slo (seen, res) sublibs
		    val seen = SrcPathSet.add (seen, p)
		in
		    case kind of
			GG.STABLELIB _ => (seen, SrcPathMap.insert (res, p, g))
		      | _ => (seen, res)
		end
	    end
	in
	    #2 (slo ((grouppath, g), (SrcPathSet.empty, SrcPathMap.empty)))
	end

	fun frontier inSet (GG.GROUP { exports, ... }) = let
	    fun bnode (DG.BNODE n, (seen, f)) = let
		val i = #bininfo n
		val li = #localimports n
	    in
		if StableSet.member (seen, i) then (seen, f)
		else let
		    val seen = StableSet.add (seen, i)
		in
		    if inSet i then (seen, StableSet.add (f, i))
		    else foldl bnode (seen, f) li
		end
	    end
	    fun get_bn (((_, DG.SB_BNODE (n, _)), _), bnl) = n :: bnl
	      | get_bn (_, bnl) = bnl
	    val bnl = SymbolMap.foldl get_bn [] exports
	in
	    #2 (foldl bnode (StableSet.empty, StableSet.empty) bnl)
	end
    end
end
