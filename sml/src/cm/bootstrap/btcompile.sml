(*
 * The bootstrap compiler.
 *   (Formerly known as "batch" compiler.)
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
functor BootstrapCompileFn (structure MachDepVC: MACHDEP_VC
			    val os: SMLofNJ.SysInfo.os_kind) = struct

    structure EM = GenericVC.ErrorMsg
    structure E = GenericVC.Environment
    structure SE = GenericVC.CMStaticEnv
    structure BE = GenericVC.BareEnvironment
    structure PS = GenericVC.PersStamps
    structure CoerceEnv = GenericVC.CoerceEnv
    structure SSV = SpecificSymValFn (structure MachDepVC = MachDepVC
				      val os = os)

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
    structure Stabilize =
	StabilizeFn (fun bn2statenv gp i = #1 (#stat (valOf (RT.bnode gp i)))
		     val recomp = recomp)
    (* ... and Parse *)
    structure Parse = ParseFn (structure Stabilize = Stabilize)

    fun compile { binroot, pcmodespec, initgspec, maingspec } = let

	val keep_going = EnvConfig.getSet StdConfig.keep_going NONE

	val ctxt = AbsPath.cwdContext ()

	val initgspec = AbsPath.native { context = ctxt, spec = initgspec }
	val maingspec = AbsPath.native { context = ctxt, spec = maingspec }
	val pcmodespec = AbsPath.native { context = ctxt, spec = pcmodespec }
	val binroot = AbsPath.native { context = ctxt, spec = binroot }
	val pidfile = AbsPath.joinDirFile { dir = binroot, file = "RTPID" }
	val listfile = AbsPath.joinDirFile { dir = binroot, file = "BINLIST" }

	val pcmode = let
	    fun work s = let
		fun loop l = let
		    val line = TextIO.inputLine s
		in
		    if line = "" then PathConfig.hardwire l
		    else case String.tokens Char.isSpace line of
			[a, s] => loop ((a, s) :: l)
		      | _ => (Say.say [AbsPath.name pcmodespec,
				       ": malformed line (ignored)\n"];
			      loop l)
		end
	    in
		loop []
	    end
	in
	    SafeIO.perform { openIt = fn () => AbsPath.openTextIn pcmodespec,
			     closeIt = TextIO.closeIn,
			     work = work,
			     cleanup = fn () => () }
	end

	val fnpolicy =
	    FilenamePolicy.separate { root = binroot,
				      parentArc = "DOTDOT",
				      absArc = "ABSOLUTE" }
	                            { arch = MachDepVC.architecture, os = os }

	fun mkParam { primconf, pervasive, pervcorepids } { corenv } =
	    { primconf = primconf,
	      fnpolicy = fnpolicy,
	      pcmode = pcmode,
	      symenv = SSV.env,
	      keep_going = keep_going,
	      pervasive = pervasive,
	      corenv = corenv,
	      pervcorepids = pervcorepids }

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

	val mkInitParam = mkParam { primconf = primconf,
				    pervasive = E.emptyEnv,
				    pervcorepids = PidSet.empty }

	val param_nocore = mkInitParam { corenv = BE.staticPart BE.emptyEnv }

	val groupreg = GroupReg.new ()
	val errcons = EM.defaultConsumer ()
	val ginfo_nocore = { param = param_nocore, groupreg = groupreg,
			     errcons = errcons }

	fun main_compile arg = let
	    val { rts, core, pervasive, primitives, binpaths } = arg

	    val ovldR = GenericVC.Control.overloadKW
	    val savedOvld = !ovldR
	    val _ = ovldR := true

	    (* here we build a new gp -- the one that uses the freshly
	     * brewed pervasive env, core env, and primitives *)
	    val core = valOf (RT.snode ginfo_nocore core)
	    val corenv =  CoerceEnv.es2bs (#1 (#stat core))

	    (* The following is a bit of a hack (but corenv is a hack anyway):
	     * As soon as we have core available, we have to patch the
	     * ginfo to include the correct corenv (because virtually
	     * everybody else needs access to corenv). *)
	    val param_justcore = mkInitParam { corenv = corenv }
	    val ginfo_justcore = { param = param_justcore, groupreg = groupreg,
				   errcons = errcons }

	    fun rt n = valOf (RT.snode ginfo_justcore n)
	    val rts = rt rts
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

	    val _ = ovldR := savedOvld

	    val param =
		mkParam { primconf = Primitive.configuration pspecs,
			  pervasive = E.mkenv { static = #1 (#stat pervasive),
					        symbolic = #1 (#sym pervasive),
						dynamic = emptydyn },
			  pervcorepids =
			    PidSet.addList (PidSet.empty,
					    [#2 (#stat pervasive),
					     #2 (#sym pervasive),
					     #2 (#stat core)]) }
		        { corenv = corenv }
	in
	    case Parse.parse param NONE maingspec of
		NONE => false
	      | SOME (g, gp) =>
		    if recomp gp g then let
			val rtspid = PS.toHex (#2 (#stat rts))
			val bootfiles =
			    map (fn x => (x, NONE)) binpaths @
			    MkBootList.group g
			fun writeList s = let
			    fun offset NONE = ["\n"]
			      | offset (SOME i) = ["@", Int.toString i, "\n"]
			    fun showBootFile (p, off) =
				TextIO.output (s,
					       concat (AbsPath.name p
						       :: offset off))
			in
			    app showBootFile bootfiles
			end
		    in
			Say.say ["Runtime System PID is: ", rtspid, "\n"];
			SafeIO.perform { openIt = fn () =>
					   AbsPath.openTextOut pidfile,
					 closeIt = TextIO.closeOut,
					 work = fn s =>
					   TextIO.output (s, rtspid ^ "\n"),
					 cleanup = fn () =>
					   AbsPath.delete pidfile };
			SafeIO.perform { openIt = fn () =>
					   AbsPath.openTextOut listfile,
					 closeIt = TextIO.closeOut,
					 work = writeList,
					 cleanup = fn () =>
					   AbsPath.delete listfile };
			true
		    end
		    else false
	end handle Option => (RT.reset (); false)
	    	   (* to catch valOf failures in "rt" *)
    in
	case BuildInitDG.build ginfo_nocore initgspec of
	    SOME x => main_compile x
	  | NONE => false
    end
end
