(*
 * Build a new "full" persistent state.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
functor FullPersstateFn (structure MachDepVC : MACHDEP_VC) :> FULL_PERSSTATE =
    struct
	structure E = GenericVC.Environment

	type env = GenericVC.Environment.dynenv

	datatype ord_key =
	    SML of SmlInfo.info
	  | STABLE of BinInfo.info

	fun compare (SML _, STABLE _) = LESS
	  | compare (STABLE _, SML _) = GREATER
	  | compare (SML i, SML i') = SmlInfo.compare (i, i')
	  | compare (STABLE i, STABLE i') = BinInfo.compare (i, i')

	structure Map =
	    BinaryMapFn (struct
		type ord_key = ord_key
		val compare = compare
	    end)

	val persmap = ref (Map.empty: env Map.map)
	val tmpmap = ref (Map.empty: env Map.map)

	local
	    fun discard_value i =
		persmap := (#1 (Map.remove (!persmap, SML i)))
		handle LibBase.NotFound => ()

	    structure RecompPersstate =
		RecompPersstateFn (structure MachDepVC = MachDepVC
				   val discard_code = false
				   val discard_value = discard_value)
	in
	    open RecompPersstate
	end

	infix o' o''
	fun (f o' g) (x, y) = f (g x, y)
	fun (f o'' g) (x, y, z) = f (g x, y, z)

	fun share (SML i) = SmlInfo.share i
	  | share (STABLE i) = BinInfo.share i

	fun exec_look (k, newCtxt, gp) = let
	    fun error (SML i) = SmlInfo.error gp i
	      | error (STABLE i) = BinInfo.error i
	    fun descr (SML i) = SmlInfo.name i
	      | descr (STABLE i) = BinInfo.describe i
	    fun didExist () = isSome (Map.find (!persmap, k))
	    fun warn_reinst () =
		if share k = SOME true andalso didExist () then
		    error k GenericVC.ErrorMsg.WARN
		            (concat ["re-instantiating ", descr k,
				     " (sharing may be lost)"])
			    GenericVC.ErrorMsg.nullErrorBody
		else ()
	in
	    case Map.find (!tmpmap, k) of
		NONE =>
		    if newCtxt then (warn_reinst (); NONE)
		    else (case Map.find (!persmap, k) of
			      NONE => NONE
			    | SOME e =>
				  if share k = SOME false then
				     (persmap := #1 (Map.remove (!persmap, k));
				      NONE)
				  else  SOME (e, false))
	      | SOME e => SOME (e, true)
	end

	val exec_look_sml = exec_look o'' SML
	val exec_look_stable = exec_look o'' STABLE

	fun exec_memo (k, e) = tmpmap := Map.insert (!tmpmap, k, e)

	val exec_memo_sml = exec_memo o' SML
	val exec_memo_stable = exec_memo o' STABLE

	fun rememberShared () = let
	    fun retainShared (k, e, m) =
		if share k = SOME false then m else Map.insert (m, k, e)
	in
	    persmap := Map.foldli retainShared (!persmap) (!tmpmap);
	    tmpmap := Map.empty
	end
    end
