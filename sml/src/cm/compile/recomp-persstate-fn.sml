(*
 * Build a new "recompilation-related" persistent state.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
local
    structure Pid = GenericVC.PersStamps
    type pid = Pid.persstamp
in
functor RecompPersstateFn
    (structure MachDepVC : MACHDEP_VC
     val new_smlinfo : SmlInfo.info -> unit
     val stable_value_present : BinInfo.info * pid option -> bool
     val discard_code : bool) :> RECOMP_PERSSTATE = struct

	structure MachDepVC = MachDepVC
	structure BF = MachDepVC.Binfile
	structure E = GenericVC.Environment

	type recomp_memo = { bfc: BF.bfContent, ctxt: E.staticEnv }
	type recomp_tmemo = recomp_memo * TStamp.t

	val smlmap = ref (SmlInfoMap.empty: recomp_tmemo SmlInfoMap.map)
	val stablemap = ref (StableMap.empty: recomp_memo StableMap.map)

	fun reset () = (smlmap := SmlInfoMap.empty;
			stablemap := StableMap.empty)

	fun transfer_state (si, bi) = let
	    val (map, (memo, _)) = SmlInfoMap.remove (!smlmap, si)
	in
	    smlmap := map;
	    stablemap := StableMap.insert (!stablemap, bi, memo)
	end

	fun recomp_look_sml (i, provided, gp) = let
	    fun isValid ({ bfc, ctxt }, ts) =
		not (TStamp.needsUpdate { source = SmlInfo.lastseen i,
					  target = ts })
		andalso let
		    val demanded =
			PidSet.addList (PidSet.empty, BF.cmDataOf bfc)
		in
		    PidSet.equal (provided, demanded)
		end
	in
	    case SmlInfoMap.find (!smlmap, i) of
		NONE => NONE
	      | SOME (memo, ts) =>
		    if isValid (memo, ts) then SOME memo
		    else (smlmap := #1 (SmlInfoMap.remove (!smlmap, i));
			  NONE)
	end

	fun recomp_memo_sml0 (i, memo) = let
	    val ts = SmlInfo.lastseen i
	    val tmemo = (memo, ts)
	in
	    new_smlinfo i;
	    smlmap := SmlInfoMap.insert (!smlmap, i, tmemo)
	end

	val recomp_memo_sml =
	    if discard_code then
		(fn x => (BF.discardCode (#bfc (#2 x));
			  recomp_memo_sml0 x))
	    else recomp_memo_sml0

	fun recomp_look_stable i = StableMap.find (!stablemap, i)
	fun recomp_memo_stable (i, memo) =
	    (if discard_code orelse
		 stable_value_present (i, BF.exportPidOf (#bfc memo))  then
		 BF.discardCode (#bfc memo)
	     else ();
	     stablemap := StableMap.insert (!stablemap, i, memo))

	fun bfc_fetch_sml i = #bfc (#1 (valOf (SmlInfoMap.find (!smlmap, i))))
	    handle Option => raise Fail "bfc_fetch_sml"
	fun bfc_fetch_stable i = #bfc (valOf (StableMap.find (!stablemap, i)))
	    handle Option => raise Fail "bfc_fetch_stable"

	val pid_fetch_sml = BF.exportPidOf o bfc_fetch_sml
    end
end