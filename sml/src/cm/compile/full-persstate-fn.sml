(*
 * Build a new "full" persistent state.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
local
    structure E = GenericVC.Environment
    structure EM = GenericVC.ErrorMsg
    structure PP = PrettyPrint
    structure DynE = DynamicEnv

    type env = GenericVC.Environment.dynenv
in
functor FullPersstateFn (structure MachDepVC : MACHDEP_VC
			 val system_values: env ref) :>
    FULL_PERSSTATE where MachDepVC = MachDepVC =
    struct
	type env = env

	datatype ord_key =
	    SML of SmlInfo.info
	  | STABLE of BinInfo.info

	fun compare (SML _, STABLE _) = LESS
	  | compare (STABLE _, SML _) = GREATER
	  | compare (SML i, SML i') = SmlInfo.compare (i, i')
	  | compare (STABLE i, STABLE i') = BinInfo.compare (i, i')

	structure K = struct
	    type ord_key = ord_key
	    val compare = compare
	end

	structure Map = BinaryMapFn (K)

	structure Set = BinarySetFn (K)

	type persentry = env * Set.set ref
	type tmpentry = env * ord_key list

	type ts = tmpentry Map.map ref

	val persmap = ref (Map.empty: persentry option Map.map)

	fun start () = ref Map.empty

	fun sh_mode (SML i) = SmlInfo.sh_mode i
	  | sh_mode (STABLE i) = BinInfo.sh_mode i

	fun discard (k, m) =
	    case Map.find (m, k) of
		NONE => m
	      | SOME NONE => m
	      | SOME (SOME (_, ref dl)) =>
		    Set.foldl discard (Map.insert (m, k, NONE)) dl

	fun discard_pers i = persmap := discard (i, !persmap)

	fun sysval NONE = NONE
	  | sysval (SOME pid) =
	    SOME (DynE.bind (pid, DynE.look (!system_values) pid,
			     DynE.empty))
	    handle DynE.Unbound => NONE

	fun stable_value_present (i, popt) =
	    isSome (sysval popt) orelse isSome (Map.find (!persmap, STABLE i))

	local
	    structure RecompPersstate =
		RecompPersstateFn (structure MachDepVC = MachDepVC
				   val discard_code = false
				   val stable_value_present =
				       stable_value_present
				   val new_smlinfo = discard_pers o SML)
	    val reset_recomp = RecompPersstate.reset
	    val transfer_state_recomp = RecompPersstate.transfer_state
	in
	    open RecompPersstate
	    fun reset () =
		(reset_recomp ();
		 persmap := Map.empty)
	    fun transfer_state (si, bi) =
		(transfer_state_recomp (si, bi);
		 discard_pers (SML si))
	end

	infix o'
	fun (f o' g) (x, y, z, w) = f (g x, y, z, w)

	fun exec_look (k, gp, popt, tmpmap: ts) = let
	    fun descr (SML i) = SmlInfo.descr i
	      | descr (STABLE i) = BinInfo.describe i
	    fun error (SML i) = SmlInfo.error gp i
	      | error (STABLE i) = BinInfo.error i
	in
	    case sysval popt of
		SOME e => SOME e
	      | NONE =>
		    (case Map.find (!tmpmap, k) of
			 NONE =>
			     (case Map.find (!persmap, k) of
				  NONE => NONE
				| SOME NONE =>
				      (case sh_mode k of
					   Sharing.SHARE true =>
					       error k EM.WARN
					        (concat ["re-instantiating ",
							 descr k,
						     " (sharing may be lost)"])
						EM.nullErrorBody
					 | _ => ();
				       NONE)
				| SOME (SOME (e, _)) =>
					   (case sh_mode k of
						Sharing.DONTSHARE =>
						    (discard_pers k; NONE)
					      | _ =>  SOME e))
		       | SOME (e, _) => SOME e)
	end

	val exec_look_sml = exec_look o' SML
	val exec_look_stable = exec_look o' STABLE

	fun exec_memo (k, e, d, tmpmap: ts) =
	    tmpmap := Map.insert (!tmpmap, k, (e, d))

	fun exec_memo_sml (i, e, sl, bl, tmpmap) =
	    exec_memo (SML i, e, map STABLE bl @ map SML sl, tmpmap)

	fun exec_memo_stable (i, e, il, tmpmap) =
	    exec_memo (STABLE i, e, map STABLE il, tmpmap)

	fun finish (tmpmap: ts) = let
	    (* We keep non-shared bindings in tmpmap; this is necessary for
	     * those partial traversals that the autoloader does.
	     * Non-shared bindings will eventually go away when the
	     * traversal state is dropped. *)
	    fun retainShared (k, (e, d), (pm, tm)) = let
		val m = discard (k, pm)
	    in
		case sh_mode k of
		    Sharing.DONTSHARE => (pm, Map.insert (tm, k, (e, d)))
		  | _ => (Map.insert (m, k, SOME (e, ref Set.empty)), tm)
	    end
	    val pm = !persmap
	    val tm = !tmpmap
	    val (pm', tm') = Map.foldli retainShared (pm, Map.empty) tm
	    fun addDep (k, (e, d)) = let
		fun addOneDep k' =
		    case Map.find (pm', k') of
			NONE => ()
		      | SOME NONE => ()
		      | SOME (SOME (_, r as ref s)) => r := Set.add (s, k)
	    in
		app addOneDep d
	    end
	in
	    tmpmap := tm';
	    persmap := pm';
	    Map.appi addDep tm
	end
    end
end
