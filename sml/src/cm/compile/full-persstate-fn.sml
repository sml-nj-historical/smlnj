(*
 * Build a new "full" persistent state.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
functor FullPersstateFn (structure MachDepVC : MACHDEP_VC) :> FULL_PERSSTATE =
    struct
	local
	    structure RecompPersstate =
		RecompPersstateFn (structure MachDepVC = MachDepVC
				   val discard_code = false)
	in
	    open RecompPersstate

	    structure E = GenericVC.Environment
	    structure DTS = DynTStamp

	    infix o'
	    fun (f o' g) (x, y) = f (g x, y)

	    type exec_memo = { dyn: E.dynenv, dts: DTS.dts }

	    val smlmap = ref (AbsPathMap.empty: exec_memo AbsPathMap.map)
	    val stablemap = ref (StableMap.empty: exec_memo StableMap.map)

	    datatype key =
		SML of SmlInfo.info
	      | STABLE of BinInfo.info

	    fun find (SML i) = AbsPathMap.find (!smlmap, SmlInfo.sourcepath i)
	      | find (STABLE i) = StableMap.find (!stablemap, i)

	    fun insert (SML i, m) =
		smlmap := AbsPathMap.insert (!smlmap, SmlInfo.sourcepath i, m)
	      | insert (STABLE i, m) =
		stablemap := StableMap.insert (!stablemap, i, m)

	    fun remove (SML i) =
		smlmap := #1 (AbsPathMap.remove (!smlmap,
						 SmlInfo.sourcepath i))
	      | remove (STABLE i) =
		stablemap := #1 (StableMap.remove (!stablemap, i))

	    fun share (SML i) = SmlInfo.share i
	      | share (STABLE i) = BinInfo.share i

	    fun error (SML i) = SmlInfo.error i
	      | error (STABLE i) = BinInfo.error i

	    fun exec_look (i, s) =
		case find i of
		    NONE => NONE
		  | SOME (memo as { dts = s', ... }) => let
			fun warn () =
			    error i GenericVC.ErrorMsg.WARN
			          "re-instantiation (sharing may be lost)"
			          GenericVC.ErrorMsg.nullErrorBody
		    in
			if DTS.outdated { context = s, oldresult = s' } then
			    (if share i = SOME true then warn () else ();
			     (remove i; NONE))
			else SOME memo
		    end

	    fun exec_memo (i, memo) =
		if share i = SOME false then () else insert (i, memo)

	    val exec_look_sml = exec_look o' SML
	    val exec_look_stable = exec_look o' STABLE
	    val exec_memo_sml = exec_memo o' SML
	    val exec_memo_stable = exec_memo o' STABLE
	end
    end
