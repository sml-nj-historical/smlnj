structure Stablize = struct

    structure DG = DependencyGraph

    datatype item =
	SS of SymbolSet.set
      | S of Symbol.symbol
      | SI of SmlInfo.info
      | AP of AbsPath.t

    fun compare (S s, S s') = SymbolOrdKey.compare (s, s')
      | compare (S _, _) = GREATER
      | compare (_, S _) = LESS
      | compare (SS s, SS s') = SymbolSet.compare (s, s')
      | compare (SS _, _) = GREATER
      | compare (_, SS _) = LESS
      | compare (SI i, SI i') = SmlInfo.compare (i, i')
      | compare (SI _, _) = GREATER
      | compare (_, SI _) = LESS
      | compare (AP p, AP p') = AbsPath.compare (p, p')

    structure Map =
	BinaryMapFn (struct
			 type ord_key = item
			 val compare = compare
	end)

    fun f (g as GroupGraph.GROUP { exports, ... }, binSizeOf, binCopy) = let
	(* The format of a stable archive is the following:
	 *  - It starts with the size s of the pickled dependency graph.
	 *    This size itself is written as four-byte string.
	 *  - The pickled dependency graph.  This graph contains integer
	 *    offsets of the binfiles for the individual ML members.
	 *    These offsets need to be adjusted by adding s + 4.
	 *    The pickled dependency graph also contains integer offsets
	 *    relative to other stable groups.  These offsets need no
	 *    further adjustment.
	 *  - Individual binfile contents (concatenated).
	 *)
	val members = let
	    fun sn (DG.SNODE { smlinfo = i, localimports = l, ... }, s) =
		if SmlInfoSet.member (s, i) then s
		else foldl sn (SmlInfoSet.add (s, i)) l
	    fun impexp (((_, DG.SB_BNODE _), _), s) = s
	      | impexp (((_, DG.SB_SNODE n), _), s) = sn (n, s)
	in
	    SmlInfoSet.listItems
	          (SymbolMap.foldl impexp SmlInfoSet.empty exports)
	end

	val offsetDict = let
	    fun add (i, (d, n)) =
		(SmlInfoMap.insert (d, i, n), n + binSizeOf i)
	in
	    #1 (foldl add (SmlInfoMap.empty, 0) members)
	end

	fun w_list w_item [] k m = ";" :: k m
	  | w_list w_item (h :: t) k m = w_item h (w_list w_item t k) m

	fun w_option w_item NONE k m = "n" :: k m
	  | w_option w_item (SOME i) k m = "s" :: w_item i k m

	fun int_encode i = let
	    (* this is the same mechanism that's also used in
	     * TopLevel/batch/binfile.sml -- maybe we should share it *)
	    val n = Word32.fromInt i
	    val // = LargeWord.div
	    val %% = LargeWord.mod
	    val !! = LargeWord.orb
	    infix // %% !!
	    val toW8 = Word8.fromLargeWord
	    fun r (0w0, l) = Word8Vector.fromList l
	      | r (n, l) = r (n // 0w128, toW8 ((n %% 0w128) !! 0w128) :: l)
	in
	    Byte.bytesToString (r (n // 0w128, [toW8 (n %% 0w128)]))
	end

	fun w_int i k m = int_encode i :: k m

	fun w_share w C v k (i, m) =
	    case Map.find (m, C v) of
		SOME i' => "o" :: w_int i' k (i, m)
	      | NONE => "n" :: w_int i (w v k) (i + 1, Map.insert (m, C v, i))

	fun w_symbol_raw s k m = SkelIO.w_name (s, k m)

	val w_symbol = w_share w_symbol_raw S

	val w_ss = w_share (w_list w_symbol o SymbolSet.listItems) SS

	val w_filter = w_option w_ss

	fun w_string s k m = let
	    fun esc #"\\" = "\\\\"
	      | esc #"\"" = "\\\""
	      | esc c = String.str c
		
	in
	    String.translate esc s :: "\"" :: k m
	end

	fun w_sharing NONE k m = "n" :: k m
	  | w_sharing (SOME true) k m = "t" :: k m
	  | w_sharing (SOME false) k m = "f" :: k m

	fun w_si_raw i k = let
	    val spec = AbsPath.spec (SmlInfo.sourcepath i)
	    val offset = valOf (SmlInfoMap.find (offsetDict, i))
	in
	    w_string spec (w_int offset (w_sharing (SmlInfo.share i) k))
	end

	val w_si = w_share w_si_raw SI

	fun w_primitive p k m = String.str (Primitive.toIdent p) :: k m

	fun w_abspath_raw p k m = w_list w_string (AbsPath.pickle p) k m

	val w_abspath = w_share w_abspath_raw AP

	fun w_bi i k = w_abspath (BinInfo.group i) (w_int (BinInfo.offset i) k)

	fun w_bn (DG.PNODE p) k m = "p" :: w_primitive p k m
	  | w_bn (DG.BNODE { bininfo, ... }) k m = "b" :: w_bi bininfo k m

	fun w_sn (DG.SNODE n) k =
	    w_si (#smlinfo n)
	         (w_list w_sn (#localimports n)
		              (w_list w_fsbn (#globalimports n) k))

	and w_sbn (DG.SB_BNODE n) = w_bn n
	  | w_sbn (DG.SB_SNODE n) = GenericVC.ErrorMsg.impossible
	    "stabilize: non-stabilized subgroup? (2)"

	and w_fsbn (f, n) k = w_filter f (w_sbn n k)

	fun w_impexp (s, (n, _)) k = w_symbol s (w_fsbn n k)

	fun w_exports e = w_list w_impexp (SymbolMap.listItemsi e)

	fun w_bool true k m = "t" :: k m
	  | w_bool false k m = "f" :: k m

	fun w_privileges p = w_list w_string (StringSet.listItems p)

	fun pickle_group (GroupGraph.GROUP g) = let
	    val { exports, islib, required, grouppath, subgroups, ... } = g
	    fun w_sg (GroupGraph.GROUP { grouppath = gp, ... }) = w_abspath gp
	    fun k0 m = []
	    val m0 = (0, Map.empty)
	in
	    concat
	      (w_exports exports
	           (w_bool islib
		          (w_privileges required
			           (w_abspath grouppath
				              (w_list w_sg subgroups k0)))) m0)
	end
	val pickle = pickle_group g
	val sz = size pickle
    in
	()
    end
end
