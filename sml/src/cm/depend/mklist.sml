(*
 * Produce a linear listing of information pertaining to nodes in
 *  a given dependency graph.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
functor MkListFn (type element
		  val bininfo : BinInfo.info -> element
		  val smlinfo : SmlInfo.info -> element) :>
sig
    val group : GroupGraph.group -> element list
end = struct

    structure DG = DependencyGraph

    fun stab_isreg ((bs, ss), i) = StableSet.member (bs, i)
    fun sml_isreg ((bs, ss), i) = SmlInfoSet.member (ss, i)
    fun stab_reg ((bs, ss), i) = (StableSet.add (bs, i), ss)
    fun sml_reg ((bs, ss), i) = (bs, SmlInfoSet.add (ss, i))

    fun do_list do_elem [] k m = k m
      | do_list do_elem (h :: t) k m = do_elem h (do_list do_elem t k) m

    fun bnode (DG.PNODE _) k m = k m
      | bnode (DG.BNODE n) k m = let
	    val { bininfo = i, localimports = l, globalimports = g } = n
	    fun k' m = bininfo i :: k (stab_reg (m, i))
	in
	    if stab_isreg (m, i) then k m
	    else do_list bnode l (do_list farbnode g k') m
	end

    and farbnode (_, n) = bnode n

    fun snode (DG.SNODE n) k m = let
	val { smlinfo = i, localimports = l, globalimports = g } = n
	fun k' m = smlinfo i :: k (sml_reg (m, i))
    in
	if sml_isreg (m, i) then k m
	else do_list snode l (do_list farsbnode g k') m
    end

    and farsbnode (_, DG.SB_BNODE (n, _)) = bnode n
      | farsbnode (_, DG.SB_SNODE n) = snode n

    fun impexp (n, _) = farsbnode n

    fun group (GroupGraph.GROUP { exports, ... }) =
	do_list impexp (SymbolMap.listItems exports)
	               (fn _ => [])
		       (StableSet.empty, SmlInfoSet.empty)
end
