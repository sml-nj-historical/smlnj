(*
 * The bootstrap compiler.
 *   (Formerly known as "batch" compiler.)
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
functor BootstrapCompileFn (structure MachDepVC: MACHDEP_VC) = struct

    structure EM = GenericVC.ErrorMsg
    structure E = GenericVC.Environment
    structure SE = GenericVC.CMStaticEnv
    structure BE = GenericVC.BareEnvironment
    structure PS = GenericVC.PersStamps
    structure CoerceEnv = GenericVC.CoerceEnv

    (* Since the bootstrap compiler never executes any of the code
     * it produces, we don't need any dynamic values.  Therefore,
     * we create RecompPersstate (but not FullPersstate!) and
     * instantiate Recomp as well as RecompTraversal.
     * Since RecompPersstate is not part of any surrounding FullPersstate,
     * function "discard_value" simply does nothing. *)
    structure RecompPersstate =
	RecompPersstateFn (structure MachDepVC = MachDepVC
			   val discard_code = true
			   fun discard_value (i: SmlInfo.info) = ())
    structure Recomp = RecompFn (structure PS = RecompPersstate)
    structure RT = CompileGenericFn (structure CT = Recomp)

    fun recomp gp g = isSome (RT.group gp g)

    (* instantiate Stabilize... *)
    structure Stabilize = StabilizeFn (fun bn2statenv gp i =
					   #1 (#stat (valOf (RT.bnode gp i)))
				       val recomp = recomp)
    (* ... and Parse *)
    structure Parse = ParseFn (structure Stabilize = Stabilize)

    fun compile (keep_going, fnpolicy, pcmode, initgspec, maingspec, sflag) = let

	val emptydyn = E.dynamicPart E.emptyEnv

	(* first, build an initial GeneralParam.info, so we can
	 * deal with the pervasive env and friends... *)
	local
	    (* We could actually go and calculate the actual pid of primEnv.
	     * But in reality it's pretty pointless to do so... *)
	    val bogusPid = PS.fromBytes (Byte.stringToBytes "0123456789abcdef")
	    val pspec = { name = "primitive",
			  env = E.mkenv { static = E.primEnv,
					  symbolic = E.symbolicPart E.emptyEnv,
					  dynamic = emptydyn },
			  pidInfo = { statpid = bogusPid,
				      sympid = bogusPid,
				      ctxt = SE.empty } }
	in
	    val primconf = Primitive.configuration [pspec]
	end

	val param = { primconf = primconf,
		      fnpolicy = fnpolicy,
		      pcmode = pcmode,
		      keep_going = keep_going,
		      pervasive = E.emptyEnv,
		      corenv = BE.staticPart BE.emptyEnv,
		      pervcorepids = PidSet.empty }

	val groupreg = GroupReg.new ()
	val errcons = EM.defaultConsumer ()
	val ginfo = { param = param, groupreg = groupreg, errcons = errcons }

	fun main_compile arg = let
	    val { rts, core, pervasive, primitives, filepaths } = arg

	    (* here we build a new gp -- the one that uses the freshly
	     * brewed pervasive env, core env, and primitives *)
	    fun rt n = valOf (RT.snode ginfo n)
	    val rts = rt rts
	    val core = rt core
	    val pervasive = rt pervasive

	    fun sn2pspec (name, n) = let
		val { stat = (s, sp), sym = (sy, syp), ctxt } = rt n
		val env =
		    E.mkenv { static = s, symbolic = sy, dynamic = emptydyn }
		val pidInfo = { statpid = sp, sympid = syp, ctxt = ctxt }
	    in
		{ name = name, env = env, pidInfo = pidInfo }
	    end

	    val pspecs = map sn2pspec primitives

	    val param = { primconf = Primitive.configuration pspecs,
			  fnpolicy = fnpolicy,
			  pcmode = pcmode,
			  keep_going = keep_going,
			  pervasive = E.mkenv { static = #1 (#stat pervasive),
					        symbolic = #1 (#sym pervasive),
						dynamic = emptydyn },
			  corenv = CoerceEnv.es2bs (#1 (#stat core)),
			  pervcorepids =
			    PidSet.addList (PidSet.empty,
					    [#2 (#stat pervasive),
					     #2 (#sym pervasive),
					     #2 (#stat core)]) }
	in
	    case Parse.parse param sflag maingspec of
		NONE => false
	      | SOME (g, gp) => recomp gp g
	end handle Option => false (* to catch valOf failures in "rt" *)
    in
	case BuildInitDG.build ginfo initgspec of
	    SOME x => main_compile x
	  | NONE => false
    end
end
