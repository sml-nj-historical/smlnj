(*
 * Check for consistency of "private" and "shared" annotations.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature CHECKSHARING = sig
    val check : GroupGraph.group * GeneralParams.info -> bool
end

structure CheckSharing :> CHECKSHARING = struct

    structure DG = DependencyGraph
    structure EM = GenericVC.ErrorMsg
    structure PP = PrettyPrint

    val empty = StringSet.empty

    fun check (GroupGraph.GROUP { exports, ... }, gp) = let

	val ok = ref true

	fun check (NONE, _, s, _) = s
	  | check (SOME false, x, s, _) = StringSet.add (s, x)
	  | check (SOME true, x, s, err) = let
		fun ppb pps = let
		    fun loop [] = ()
		      | loop (h :: t) =
			(PP.add_string pps h;
			 PP.add_newline pps;
			 loop t)
		in
		    PP.add_newline pps;
		    PP.add_string pps
		       "because of dependence on non-shareable state in:";
		    PP.add_newline pps;
		    loop (StringSet.listItems s)
		end
	    in
		if StringSet.isEmpty s then ()
		else (err EM.COMPLAIN ("cannot share state of " ^ x) ppb;
		      ok := false);
		StringSet.add (s, x)
	    end

	val smlmap = ref SmlInfoMap.empty
	val stablemap = ref StableMap.empty

	fun bn (DG.PNODE _, s) = s
	  | bn (DG.BNODE { bininfo = i, localimports, globalimports }, s) =
	    case StableMap.find (!stablemap, i) of
		SOME s' => StringSet.union (s, s')
	      | NONE => let
		    val gs = foldl fbn empty globalimports
		    val ls = foldl bn gs localimports
		    val s' = check (BinInfo.share i, BinInfo.describe i, ls,
				    BinInfo.error gp i)
		in
		    stablemap := StableMap.insert (!stablemap, i, s');
		    StringSet.union (s, s')
		end

	and fbn ((_, n), s) = bn (n, s)

	fun sn (DG.SNODE { smlinfo = i, localimports, globalimports }, s) =
	    case SmlInfoMap.find (!smlmap, i) of
		SOME s' => StringSet.union (s, s')
	      | NONE => let
		    val gs = foldl fsbn empty globalimports
		    val ls = foldl sn gs localimports
		    val s' = check (SmlInfo.share i, SmlInfo.name i, ls,
				    SmlInfo.error gp i)
		in
		    smlmap := SmlInfoMap.insert (!smlmap, i, s');
		    StringSet.union (s, s')
		end

	and sbn (DG.SB_BNODE n, s) = bn (n, s)
	  | sbn (DG.SB_SNODE n, s) = sn (n, s)

	and fsbn ((_, n), s) = sbn (n, s)

	fun impexp (n, _) = ignore (fsbn (n, StringSet.empty))
    in
	SymbolMap.app impexp exports;
	!ok
    end
end
