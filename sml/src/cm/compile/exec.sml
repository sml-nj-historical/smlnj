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
    structure DE = GenericVC.DynamicEnv
    structure BF = PS.MachDepVC.Binfile
    structure PP = PrettyPrint
    structure EM = GenericVC.ErrorMsg

    type env = (unit -> E.dynenv) * bool
    type benv = env
    type envdelta = env

    fun layer ((d, n), (d', n')) = (fn () => DE.atop (d (), d'()), n orelse n')

    fun filter (e, _) = e
    fun nofilter e = e

    val blayer = layer
    val bfilter = filter
    val bnofilter = nofilter

    fun primitive (gp: GeneralParams.info) p =
	(fn () => E.dynamicPart (Primitive.env (#primconf (#param gp)) p),
	 false)

    fun pervasive (gp: GeneralParams.info) =
	(fn () => E.dynamicPart (#pervasive (#param gp)), false)

    val bpervasive = pervasive

    fun thunkify (d, n) = (fn () => d, n)

    fun execute (bfc, (mkdyn, newCtxt), error, descr, memo) = let
	val e = BF.exec (bfc, mkdyn ())
	val de = E.dynamicPart e
    in
	memo de;
	SOME (thunkify (de, newCtxt))
    end handle exn => let
	fun ppb pps =
	    (PP.add_newline pps;
	     PP.add_string pps (General.exnMessage exn);
	     PP.add_newline pps)
    in
	error ("link-time error in " ^ descr) ppb;
	NONE
    end

    fun dostable (i, mkenv, gp) =
	case mkenv () of
	    NONE => NONE
	  | SOME (e as (dyn, newCtxt)) =>
		(case PS.exec_look_stable (i, newCtxt, gp) of
		     SOME m => SOME (thunkify m)
		   | NONE => execute (PS.bfc_fetch_stable i, e,
				      BinInfo.error i EM.COMPLAIN,
				      BinInfo.describe i,
				      fn e => PS.exec_memo_stable (i, e)))

    fun dosml (i, e as (dyn, newCtxt), gp) =
	case PS.exec_look_sml (i, newCtxt, gp) of
	    SOME m => SOME (thunkify m)
	  | NONE => execute (PS.bfc_fetch_sml i, e,
			     SmlInfo.error gp i EM.COMPLAIN,
			     SmlInfo.name i,
			     fn m => PS.exec_memo_sml (i, m))
end
