(*
 * Check for consistency of "private" and "shared" annotations.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
local
    structure DG = DependencyGraph
    structure EM = GenericVC.ErrorMsg
    structure PP = PrettyPrint
in
  signature CHECKSHARING = sig
    val check : DG.impexp SymbolMap.map * GeneralParams.info -> unit
  end

  structure CheckSharing :> CHECKSHARING = struct

    fun check (exports, gp) = let

	fun check (Sharing.DONTCARE, _, s, _) =
	    (s, if StringSet.isEmpty s then Sharing.SHARE false
		else Sharing.DONTSHARE)
	  | check (Sharing.PRIVATE, x, _, _) =
	    (StringSet.singleton x, Sharing.DONTSHARE)
	  | check (Sharing.SHARED, x, s, err) = let
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
		if StringSet.isEmpty s then (s, Sharing.SHARE true)
		else (err EM.COMPLAIN ("cannot share state of " ^ x) ppb;
		      (s, Sharing.DONTSHARE))
	    end

	val smlmap = ref SmlInfoMap.empty

	fun bn (DG.PNODE _) = StringSet.empty
	  | bn (DG.BNODE { bininfo = i, ... }) =
	    case BinInfo.sh_mode i of
		Sharing.DONTSHARE => StringSet.singleton (BinInfo.describe i)
	      | _ => StringSet.empty

	fun sn (DG.SNODE n) = let
	    val { smlinfo = i, localimports = li, globalimports = gi, ... } = n
	    fun acc f (arg, s) = StringSet.union (f arg, s)
	in
	    case SmlInfoMap.find (!smlmap, i) of
		SOME s => s
	      | NONE => let
		    val gs = foldl (acc fsbn) StringSet.empty gi
		    val ls = foldl (acc sn) gs li
		    val (s, m) = check (SmlInfo.sh_spec i, SmlInfo.descr i, ls,
					SmlInfo.error gp i)
		in
		    smlmap := SmlInfoMap.insert (!smlmap, i, s);
		    SmlInfo.set_sh_mode (i, m);
		    s
		end
	end

	and sbn (DG.SB_BNODE n) = bn n
	  | sbn (DG.SB_SNODE n) = sn n

	and fsbn (_, n) = sbn n

	fun impexp (n, _) = ignore (fsbn n)
    in
	SymbolMap.app impexp exports
    end
  end
end
