(*
 * Get the set of reachable SNODEs in a given dependency graph.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature REACHABLE = sig
    val reachable' : DependencyGraph.impexp SymbolMap.map -> SrcPathSet.set
    val reachable : GroupGraph.group -> SrcPathSet.set
end

structure Reachable :> REACHABLE = struct
    structure DG = DependencyGraph

    fun reachable' exports = let
	fun snode (DG.SNODE n, known) = let
	    val { smlinfo, localimports = l, globalimports = g } = n
	    val p = SmlInfo.sourcepath smlinfo
	in
	    if SrcPathSet.member (known, p) then known
	    else foldl globi (foldl loci (SrcPathSet.add (known, p)) l) g
	end

	and loci ((n, _), known) = snode (n, known)
	and globi ((n, _), known) = farsbnode (n, known)

	and farsbnode ((_, n), known) = sbnode (n, known)

	and sbnode (DG.SB_BNODE _, known) = known
	  | sbnode (DG.SB_SNODE n, known) = snode (n, known)

	fun impexp ((n, _), known) = farsbnode (n, known)
    in
	SymbolMap.foldl impexp SrcPathSet.empty exports
    end

    fun reachable (GroupGraph.GROUP { exports, ... }) = reachable' exports
end
