(*
 * The bootstrap compiler.
 *   (Formerly known as "batch" compiler.)
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
functor BootstrapCompileFn (structure MachDepVC: MACHDEP_VC
			    val os: SMLofNJ.SysInfo.os_kind) :> sig
    val make' : string option -> bool
    val make : unit -> bool
    val deliver' : string option -> bool
    val deliver : unit -> bool
    val reset : unit -> unit
end = struct

    structure EM = GenericVC.ErrorMsg
    structure E = GenericVC.Environment
    structure SE = GenericVC.CMStaticEnv
    structure BE = GenericVC.BareEnvironment
    structure PS = GenericVC.PersStamps
    structure CoerceEnv = GenericVC.CoerceEnv
    structure SSV = SpecificSymValFn (structure MachDepVC = MachDepVC
				      val os = os)
    structure P = OS.Path
    structure F = OS.FileSys

    structure Compile = CompileFn (structure MachDepVC = MachDepVC)

    (* instantiate Stabilize... *)
    structure Stabilize =
	StabilizeFn (val writeBFC = Compile.writeBFC
		     val sizeBFC = Compile.sizeBFC
		     val getII = Compile.getII
		     fun destroy_state _ _ = ()
		     fun recomp gp g = let
			 val { group, ... } =
			     Compile.newTraversal (fn _ => fn _ => (), g)
		     in
			 isSome (group gp)
		     end)

    (* ... and Parse *)
    structure Parse = ParseFn (structure Stabilize = Stabilize
			       fun pending () = SymbolMap.empty)

    (* copying an input file to an output file safely... *)
    fun copyFile (oi, ci, oo, co, inp, outp, eof) (inf, outf) = let
	fun workIn is = let
	    fun workOut os = let
		val N = 4096
		fun loop () =
		    if eof is then () else (outp (os, inp (is, N)); loop ())
	    in
		loop ()
	    end
	in
	    SafeIO.perform { openIt = fn () => oo outf,
			     closeIt = co,
			     work = workOut,
			     cleanup = fn () =>
			         (F.remove outf handle _ => ()) }
	end
    in
	SafeIO.perform { openIt = fn () => oi inf,
			 closeIt = ci,
			 work = workIn,
			 cleanup = fn () => () }
    end

    val copyTextFile =
	copyFile (TextIO.openIn, TextIO.closeIn,
		  AutoDir.openTextOut, TextIO.closeOut,
		  TextIO.inputN, TextIO.output, TextIO.endOfStream)

    val copyBinFile =
	copyFile (BinIO.openIn, BinIO.closeIn,
		  AutoDir.openBinOut, BinIO.closeOut,
		  BinIO.inputN, BinIO.output, BinIO.endOfStream)

    fun compile deliver dbopt = let

	val dirbase = getOpt (dbopt, BtNames.dirbaseDefault)
	val pcmodespec = BtNames.pcmodespec
	val initgspec = BtNames.initgspec
	val maingspec = BtNames.maingspec

	val arch = MachDepVC.architecture
	val osname = FilenamePolicy.kind2name os
	val bindir = concat [dirbase, ".bin.", arch, "-", osname]
	val bootdir = concat [dirbase, ".boot.", arch, "-", osname]

	fun listName (p, copy) =
	    case P.fromString p of
		{ vol = "", isAbs = false, arcs = arc0 :: arc1 :: arcn } => let
		    fun win32name () =
			concat (arc1 ::
				foldr (fn (a, r) => "\\" :: a :: r) [] arcn)
		    fun doCopy () = let
			val bootpath =
			    P.toString { isAbs = false, vol = "",
					 arcs = bootdir :: arc1 :: arcn }
		    in
			copyBinFile (p, bootpath)
		    end
		in
		    if copy andalso arc0 = bindir then doCopy () else ();
		    case os of
			SMLofNJ.SysInfo.WIN32 => win32name ()
		      | _ => P.toString { isAbs = false, vol = "",
					  arcs = arc1 :: arcn }
		end
	      | _ => raise Fail "BootstrapCompile:listName: bad name"

	val keep_going = EnvConfig.getSet StdConfig.keep_going NONE

	val ctxt = SrcPath.cwdContext ()

	val pidfile = P.joinDirFile { dir = bootdir, file = "RTPID" }
	val listfile = P.joinDirFile { dir = bootdir, file = "BOOTLIST" }

	val pcmode = PathConfig.new ()
	val _ = PathConfig.processSpecFile (pcmode, pcmodespec)

	fun stdpath s = SrcPath.standard pcmode { context = ctxt, spec = s }

	val initgspec = stdpath initgspec
	val maingspec = stdpath maingspec

	val cmifile = valOf (SrcPath.reAnchoredName (initgspec, bootdir))
	    handle Option => raise Fail "BootstrapCompile: cmifile"

	val fnpolicy =
	    FilenamePolicy.separate { bindir = bindir, bootdir = bootdir }
	        { arch = arch, os = os }

	fun mkParam { primconf, pervasive, pervcorepids }
	            { corenv } =
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

	val primconf = Primitive.primEnvConf
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
	    val sbnode = Compile.newSbnodeTraversal ()

	    (* here we build a new gp -- the one that uses the freshly
	     * brewed pervasive env, core env, and primitives *)
	    val core = valOf (sbnode ginfo_nocore core)
	    val corenv =  CoerceEnv.es2bs (#statenv (#ii core) ())
	    val core_sym = #symenv (#ii core) ()

	    (* The following is a bit of a hack (but corenv is a hack anyway):
	     * As soon as we have core available, we have to patch the
	     * ginfo to include the correct corenv (because virtually
	     * everybody else needs access to corenv). *)
	    val param_justcore = mkInitParam { corenv = corenv }
	    val ginfo_justcore = { param = param_justcore, groupreg = groupreg,
				   errcons = errcons }

	    fun rt n = valOf (sbnode ginfo_justcore n)
	    val rts = rt rts
	    val pervasive = rt pervasive

	    fun sn2pspec (name, n) = let
		val { ii = { statenv, symenv, statpid, sympid }, ctxt } = rt n
		val env =
		    E.mkenv { static = statenv (),
			      symbolic = symenv (),
			      dynamic = emptydyn }
		val pidInfo =
		    { statpid = statpid, sympid = sympid, ctxt = ctxt }
	    in
		{ name = name, env = env, pidInfo = pidInfo }
	    end

	    val pspecs = map sn2pspec primitives

	    val _ = ovldR := savedOvld

	    (* The following is a hack but must be done for both the symbolic
	     * and later the dynamic part of the core environment:
	     * we must include these parts in the pervasive env. *)
	    val perv_sym = E.layerSymbolic (#symenv (#ii pervasive) (),
					    core_sym)

	    val param =
		mkParam { primconf = Primitive.configuration pspecs,
			  pervasive = E.mkenv { static =
					         #statenv (#ii pervasive) (),
					        symbolic = perv_sym,
						dynamic = emptydyn },
			  pervcorepids =
			    PidSet.addList (PidSet.empty,
					    [#statpid (#ii pervasive),
					     #sympid (#ii pervasive),
					     #statpid (#ii core)]) }
		        { corenv = corenv }
	    val stab =
		if deliver then SOME true else NONE
	in
	    case Parse.parse NONE param stab maingspec of
		NONE => false
	      | SOME (g, gp) => let
		    val { group = recomp, ... } =
			Compile.newTraversal (fn _ => fn _ => (), g)
		in
		    if isSome (recomp gp) then let
			val rtspid = PS.toHex (#statpid (#ii rts))
			fun writeList s = let
			    fun add ((p, flag), l) = let
				val n = listName (p, true)
			    in
				if flag then n :: l else l
			    end
			    fun transcribe (p, NONE) = listName (p, true)
			      | transcribe (p, SOME (off, desc)) =
				concat [listName (p, false),
					"@", Int.toString off, ":", desc]
			    val bootstrings =
				foldr add (map transcribe (MkBootList.group g))
				      binpaths
			    fun show str =
				(TextIO.output (s, str);
				 TextIO.output (s, "\n"))
			in
			    app show bootstrings
			end
		    in
		      if deliver then
		       (SafeIO.perform { openIt = fn () =>
					   AutoDir.openTextOut pidfile,
					 closeIt = TextIO.closeOut,
					 work = fn s =>
					   TextIO.output (s, rtspid ^ "\n"),
					 cleanup = fn () =>
					   OS.FileSys.remove pidfile
					   handle _ => () };
			SafeIO.perform { openIt = fn () =>
					   AutoDir.openTextOut listfile,
					 closeIt = TextIO.closeOut,
					 work = writeList,
					 cleanup = fn () =>
					   OS.FileSys.remove listfile
					   handle _ => () };
			copyTextFile (SrcPath.osstring initgspec, cmifile);
			Say.say ["Runtime System PID is: ", rtspid, "\n"])
		      else ();
		      true
		    end
		    else false
		end
	end handle Option => (Compile.reset (); false)
	    	   (* to catch valOf failures in "rt" *)
    in
	case BuildInitDG.build ginfo_nocore initgspec of
	    SOME x => main_compile x
	  | NONE => false
    end

    fun reset () =
	(Compile.reset ();
	 Parse.reset ())

    val make' = compile false
    fun make () = make' NONE
    fun deliver' arg =
	SafeIO.perform { openIt = fn () => (),
			 closeIt = reset,
			 work = fn () => compile true arg,
			 cleanup = fn () => () }
    fun deliver () = deliver' NONE
end
