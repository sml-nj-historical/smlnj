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

    type env = (unit -> E.dynenv) * SmlInfo.info list * BinInfo.info list
    type benv = env
    type envdelta = env
    type result = E.dynenv

    fun reset () = ()

    fun layer ((d, sl, bl), (d', sl', bl')) =
	(fn () => DE.atop (d (), d' ()), sl @ sl', bl @ bl')

    fun filter (e, _) = e
    fun nofilter e = e

    val blayer = layer
    val bfilter = filter
    val bnofilter = nofilter

    val empty = DE.empty
    fun env2result ((mkEnv, _, _): env) = mkEnv ()
    fun rlayer (r, r') = DE.atop (r, r')

    fun primitive (gp: GeneralParams.info) p =
	(fn () => E.dynamicPart (Primitive.env (#primconf (#param gp)) p),
	 [], [])

    fun pervasive (gp: GeneralParams.info) =
	(fn () => E.dynamicPart (#pervasive (#param gp)),
	 [], [])

    val bpervasive = pervasive

    fun thunkify d () = d

    fun execute (bfc, mkdyn, error, descr, memo, sl, bl) = let
	val e = BF.exec (bfc, mkdyn ())
	val de = E.dynamicPart e
    in
	memo de;
	SOME (thunkify de, sl, bl)
    end handle exn => let
	fun ppb pps =
	    (PP.add_newline pps;
	     PP.add_string pps (General.exnMessage exn);
	     PP.add_newline pps)
    in
	error ("link-time error in " ^ descr) ppb;
	NONE
    end

    fun dostable (i, mkbenv, gp) =
	case mkbenv () of
	    NONE => NONE
	  | SOME (benv, sl, bl) =>
		(case PS.exec_look_stable (i, gp) of
		     SOME m => SOME (thunkify m, [], [i])
		   | NONE => (execute (PS.bfc_fetch_stable i, benv,
				       BinInfo.error i EM.COMPLAIN,
				       BinInfo.describe i,
				       fn e => PS.exec_memo_stable (i, e, bl),
				       [], [i])))
			 
    fun fetch_sml i =
	PS.bfc_fetch_sml i handle e => (print "!!! fetch_sml\n"; raise e)

    fun dosml (i, (env, sl, bl), gp) =
	case PS.exec_look_sml (i, gp) of
	    SOME m => SOME (thunkify m, [i], [])
	  | NONE => (execute (fetch_sml i, env,
			      SmlInfo.error gp i EM.COMPLAIN,
			      SmlInfo.descr i,
			      fn m => PS.exec_memo_sml (i, m, sl, bl),
			      [i], []))
end
