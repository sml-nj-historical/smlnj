(*
 * Get the set of reachable SNODEs in a given dependency graph.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature REACHABLE = sig
    val reachable : GroupGraph.group -> SrcPathSet.set
end

structure Reachable :> REACHABLE = struct
    structure DG = DependencyGraph

    fun reachable (GroupGraph.GROUP { exports, ... }) = let
	fun snode (DG.SNODE n, known) = let
	    val { smlinfo, localimports = l, globalimports = g } = n
	    val p = SmlInfo.sourcepath smlinfo
	in
	    if SrcPathSet.member (known, p) then known
	    else foldl farsbnode (foldl snode (SrcPathSet.add (known, p)) l) g
	end

	and farsbnode ((_, n), known) = sbnode (n, known)

	and sbnode (DG.SB_BNODE _, known) = known
	  | sbnode (DG.SB_SNODE n, known) = snode (n, known)

	fun impexp ((n, _), known) = farsbnode (n, known)
    in
	SymbolMap.foldl impexp SrcPathSet.empty exports
    end
end
