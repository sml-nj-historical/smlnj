(*
 * Build a new "recompilation-related" persistent state.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
functor RecompPersstateFn (structure MachDepVC : MACHDEP_VC
			   val discard_code : bool) :> RECOMP_PERSSTATE =
    struct
	structure MachDepVC = MachDepVC
	structure BF = MachDepVC.Binfile
	structure E = GenericVC.Environment

	type recomp_memo = { bfc: BF.bfContent, ctxt: E.staticEnv }

	val smlmap = ref (AbsPathMap.empty: recomp_memo AbsPathMap.map)
	val stablemap = ref (StableMap.empty: recomp_memo StableMap.map)

	fun recomp_look_sml (i, provided, gp) = let
	    fun isValid { bfc, ctxt } = let
		val demanded = PidSet.addList (PidSet.empty, BF.cmDataOf bfc)
	    in
		PidSet.equal (provided, demanded)
	    end
	    val p = SmlInfo.sourcepath i
	in
	    case AbsPathMap.find (!smlmap, p) of
		NONE => NONE
	      | SOME memo =>
		    if isValid memo then SOME memo
		    else (smlmap := #1 (AbsPathMap.remove (!smlmap, p));
			  NONE)
	end

	fun recomp_memo_sml0 (i, memo) =
	    smlmap := AbsPathMap.insert (!smlmap, SmlInfo.sourcepath i, memo)

	fun discard (arg as (_, { bfc, ctxt })) = (BF.discardCode bfc; arg)

	val recomp_memo_sml =
	    if discard_code then recomp_memo_sml0 o discard
	    else recomp_memo_sml0

	fun recomp_look_stable i = StableMap.find (!stablemap, i)
	fun recomp_memo_stable0 (i, memo) =
	    stablemap := StableMap.insert (!stablemap, i, memo)

	val recomp_memo_stable =
	    if discard_code then recomp_memo_stable0 o discard
	    else recomp_memo_stable0

	fun bfc_fetch_sml i =
	    #bfc (valOf (AbsPathMap.find (!smlmap, SmlInfo.sourcepath i)))
	fun bfc_fetch_stable i =
	    #bfc (valOf (StableMap.find (!stablemap, i)))
    end
