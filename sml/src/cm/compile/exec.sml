(*
 * Build an argument for the generic compilation functor.
 *   This gives a traversal that executes the code in each node as
 *   necessary (and builds the dynamic environment).
 *   A traversal requires prior successful traversal using the
 *   "RecompFn" functor (using the same persistent state).
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
functor ExecFn (structure PS : FULL_PERSSTATE) : COMPILATION_TYPE = struct

    structure E = GenericVC.Environment
    structure DTS = DynTStamp
    structure DE = GenericVC.DynamicEnv
    structure BF = PS.MachDepVC.Binfile
    structure PP = PrettyPrint
    structure EM = GenericVC.ErrorMsg

    type env = { dyn: unit -> E.dynenv, dts: DTS.dts }
    type benv = env
    type envdelta = env

    fun layer ({ dyn = d, dts = s }, { dyn = d', dts = s' }) =
	{ dyn = fn () => DE.atop (d (), d' ()), dts = DTS.join (s, s') }

    fun filter (e, _) = e
    fun nofilter e = e

    val blayer = layer
    val bfilter = filter
    val bnofilter = nofilter

    fun primitive (gp: GeneralParams.info) p =
	{ dyn = fn () => E.dynamicPart (Primitive.env
					(#primconf (#param gp)) p),
	  dts = DTS.ancient }

    fun pervasive (gp: GeneralParams.info) =
	{ dyn = fn () => E.dynamicPart (#pervasive (#param gp)),
	  dts = DTS.ancient }

    val bpervasive = pervasive

    fun thunkify { dyn, dts } = { dyn = fn () => dyn, dts = dts }

    fun execute (bfc, { dyn = mkdyn, dts }, share, error, descr, memo) = let
	val (tryshare, mustshare) =
	    case share of
		NONE => (true, false)
	      | SOME true => (true, true)
	      | SOME false => (false, false)
	fun doit () = let
	    val dts' = if tryshare then DTS.current ()
		       else DTS.noshare descr
	    val e = BF.exec (bfc, mkdyn ())
	    val m = { dyn = E.dynamicPart e, dts = DTS.join (dts, dts') }
	in
	    memo m;
	    SOME (thunkify m)
	end handle exn => let
	    fun pphist pps =
		(PP.add_string pps (General.exnMessage exn);
		 PP.add_newline pps)
	in
	    error "exception in module initialization code" pphist;
	    NONE
	end
    in
	if mustshare then
	    case DTS.can'tShare dts of
		NONE => doit ()
	      | SOME sl => let
		    fun pphist [] pps = PP.add_newline pps
		      | pphist (h :: t) pps =
			(PP.add_newline pps;
			 PP.add_string pps h;
			 pphist t pps)
		in
		    error
		      "cannot share state: dependence on non-shareable modules"
		      (pphist sl);
		    NONE
		end
	else doit ()
    end

    fun dostable (i, mkenv, gp) =
	case mkenv () of
	    NONE => NONE
	  | SOME (e as { dyn, dts }) =>
		(case PS.exec_look_stable (i, dts, gp) of
		     SOME memo => SOME (thunkify memo)
		   | NONE => execute (PS.bfc_fetch_stable i, e,
				      BinInfo.share i,
				      BinInfo.error gp i EM.COMPLAIN,
				      BinInfo.describe i,
				      fn m => PS.exec_memo_stable (i, m)))

    fun dosml (i, e as { dyn, dts }, gp) = let
	fun looksml () =
	    Option.map thunkify (PS.exec_look_sml (i, dts, gp))
    in
	case looksml () of
	    SOME d => SOME d
	  | NONE => execute (PS.bfc_fetch_sml i, e,
			     SmlInfo.share i,
			     SmlInfo.error gp i EM.COMPLAIN,
			     SmlInfo.name i,
			     fn m => PS.exec_memo_sml (i, m))
    end
end
