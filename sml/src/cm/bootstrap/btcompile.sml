(*
 * The bootstrap compiler.
 *   (Formerly known as "batch" compiler.)
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
functor BootstrapCompileFn (structure MachDepVC : MACHDEP_VC
			    val os : SMLofNJ.SysInfo.os_kind
			    val load_plugin : string -> bool) :> sig
    val make' : string option -> bool
    val make : unit -> bool
    val deliver' : string option -> bool
    val deliver : unit -> bool
    val reset : unit -> unit
    val symval : string -> { get: unit -> int option, set: int option -> unit }
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
    structure BF = MachDepVC.Binfile

    val arch = MachDepVC.architecture
    val osname = FilenamePolicy.kind2name os
    val archos = concat [arch, "-", osname]

    fun init_servers (GroupGraph.GROUP { grouppath, ... }) =
	Servers.cmb { archos = archos,
		      root = SrcPath.descr grouppath }

    structure Compile = CompileFn (structure MachDepVC = MachDepVC
				   val compile_there =
				       Servers.compile o SrcPath.descr)

    structure BFC = BfcFn (structure MachDepVC = MachDepVC)

    (* instantiate Stabilize... *)
    structure Stabilize =
	StabilizeFn (structure MachDepVC = MachDepVC
		     fun recomp gp g = let
			 val { store, get } = BFC.new ()
			 val _ = init_servers g
			 val { group, ... } =
			     Compile.newTraversal (fn _ => fn _ => (),
						   store, g)
		     in
			 case Servers.withServers (fn () => group gp) of
			     NONE => NONE
			   | SOME _ => SOME get
		     end
		     val getII = Compile.getII)

    (* ... and Parse *)
    structure Parse = ParseFn (structure Stabilize = Stabilize
			       val evictStale = Compile.evictStale
			       fun pending () = SymbolMap.empty)

    fun mkBootList g = let
	fun listName p =
	    case P.fromString p of
		{ vol = "", isAbs = false, arcs = _ :: arc1 :: arcn } => let
		    fun win32name () =
			concat (arc1 ::
				foldr (fn (a, r) => "\\" :: a :: r) [] arcn)
		in
		    case os of
			SMLofNJ.SysInfo.WIN32 => win32name ()
		      | _ => P.toString { isAbs = false, vol = "",
					  arcs = arc1 :: arcn }
		end
	      | _ => raise Fail ("BootstrapCompile:listName: bad name: " ^ p)
    in
	MkBootList.group listName g
    end

    fun mk_compile deliver root dbopt = let

	val dirbase = getOpt (dbopt, BtNames.dirbaseDefault)
	val pcmodespec = BtNames.pcmodespec
	val initgspec = BtNames.initgspec
	val maingspec = BtNames.maingspec

	val bindir = concat [dirbase, BtNames.bin_infix, archos]
	val bootdir = concat [dirbase, BtNames.boot_infix, archos]

	val keep_going = #get StdConfig.keep_going ()

	val ctxt = SrcPath.cwdContext ()

	val listfile = P.joinDirFile { dir = bootdir, file = BtNames.bootlist }
	val pidmapfile = P.joinDirFile { dir = bootdir, file = BtNames.pidmap }

	val pcmode = PathConfig.new ()
	val _ = PathConfig.processSpecFile (pcmode, pcmodespec)

	fun stdpath s = SrcPath.standard pcmode { context = ctxt, spec = s }

	val initgspec = stdpath initgspec
	val maingspec =
	    case root of
		NONE => stdpath maingspec
	      | SOME r => SrcPath.fromDescr pcmode r

	val fnpolicy =
	    FilenamePolicy.separate { bindir = bindir, bootdir = bootdir }
	        { arch = arch, os = os }

	fun mkParam corenv =
	    { fnpolicy = fnpolicy,
	      pcmode = pcmode,
	      symval = SSV.symval,
	      keep_going = keep_going,
	      corenv = corenv }

	val emptydyn = E.dynamicPart E.emptyEnv

	(* first, build an initial GeneralParam.info, so we can
	 * deal with the pervasive env and friends... *)

	val param_nocore = mkParam BE.emptyEnv

	val groupreg = GroupReg.new ()
	val errcons = EM.defaultConsumer ()
	val ginfo_nocore = { param = param_nocore, groupreg = groupreg,
			     errcons = errcons }

	fun mk_main_compile arg = let

	    val { core = core_n, pervasive = perv_n, others, src } = arg

	    val ovldR = GenericVC.Control.overloadKW
	    val savedOvld = !ovldR
	    val _ = ovldR := true
	    val sbnode = Compile.newSbnodeTraversal ()

	    (* here we build a new gp -- the one that uses the freshly
	     * brewed pervasive env, core env, and primitives *)
	    val core = valOf (sbnode ginfo_nocore core_n)
	    val corenv =
		BE.mkenv { static = CoerceEnv.es2bs (#env (#statenv core ())),
			   symbolic = #symenv core (),
			   dynamic = BE.dynamicPart BE.emptyEnv }

	    (* The following is a bit of a hack (but corenv is a hack anyway):
	     * As soon as we have core available, we have to patch the
	     * ginfo to include the correct corenv (because virtually
	     * everybody else needs access to corenv). *)
	    val param = mkParam corenv
	    val ginfo =
		{ param = param, groupreg = groupreg, errcons = errcons }

	    val perv_fsbnode = (NONE, perv_n)

	    fun rt n = valOf (sbnode ginfo n)
	    val pervasive = rt perv_n

	    fun rt2ie (n, ii: IInfo.info) = let
		val bs = CoerceEnv.es2bs (#env (#statenv ii ()))
		val (dae, mkDomain) = Statenv2DAEnv.cvt bs
	    in
		{ ie = ((NONE, n), dae), mkDomain = mkDomain }
	    end
		
	    fun add_exports (n, exports) = let
		val { ie, mkDomain } = rt2ie (n, rt n)
		fun ins_ie (sy, m) = SymbolMap.insert (m, sy, ie)
	    in
		SymbolSet.foldl ins_ie exports (mkDomain ())
	    end

	    val special_exports = let
		fun mkie (n, rtn) = #ie (rt2ie (n, rtn))
	    in
		foldl SymbolMap.insert' SymbolMap.empty
		      [(PervCoreAccess.pervStrSym, mkie (perv_n, pervasive)),
		       (PervCoreAccess.coreStrSym, mkie (core_n, core))]
	    end

	    val init_group = GroupGraph.GROUP
		{ exports = foldl add_exports special_exports others,
		  kind = GroupGraph.LIB (StringSet.empty, []),
		  required = StringSet.singleton "primitive",
		  grouppath = initgspec,
		  sublibs = [] }

	    val _ = ovldR := savedOvld

	    (* At this point we check if there is a usable stable version
	     * of the init group.  If so, we continue to use that. *)
	    val (stab, init_group, deliver) = let
		fun nostabinit () =
		    if deliver then
			let val stabarg = { group = init_group,
					    anyerrors = ref false }
			in
			    case Stabilize.stabilize ginfo stabarg of
				SOME g => (SOME true, g, true)
			      (* if we cannot stabilize the init group, then
			       * as a first remedy we turn delivery off *)
			      | NONE => (NONE, init_group, false)
			end
		    else (NONE, init_group, false)
	    in
		if VerifyStable.verify ginfo init_group then let
		    fun load () =
			Stabilize.loadStable ginfo
			  { getGroup = fn _ =>
			       raise Fail "CMB: initial getGroup",
			    anyerrors = ref false }
			  initgspec
		in
		    case load () of
			NONE => nostabinit ()
		      | SOME g =>
			    (if deliver then SOME true else NONE, g, deliver)
		end
		else nostabinit ()
	    end

	    val gr = GroupReg.new ()
	    val _ = GroupReg.register gr (initgspec, src)

	    val parse_arg =
		{ load_plugin = load_plugin,
		  gr = gr,
		  param = param,
		  stabflag = stab,
		  group = maingspec,
		  init_group = init_group,
		  paranoid = true }
	in
	    Servers.dirbase dirbase;
	    Parse.reset ();
	    case Parse.parse parse_arg of
		NONE => NONE
	      | SOME (g, gp) => let
		    fun thunk () = let
			val _ = init_servers g
			fun store _ = ()
			val { group = recomp, ... } =
			    Compile.newTraversal (fn _ => fn _ => (), store, g)
			val res =
			    Servers.withServers (fn () => recomp gp)
		    in
			if isSome res then let
			    val { l = bootitems, ss } = mkBootList g
			    val stablelibs = Reachable.stableLibsOf g
			    fun inSet bi = StableSet.member (ss, bi)
			    val frontiers =
				SrcPathMap.map (Reachable.frontier inSet)
				               stablelibs
			    fun writeBootList s = let
				fun wr str = TextIO.output (s, str ^ "\n")
			    in
				app wr bootitems
			    end
			    fun writePid s i = let
				val sn = BinInfo.stablename i
				val os = BinInfo.offset i
				val descr = BinInfo.describe i
				val bfc = BFC.getStable
				    { stable = sn, offset = os, descr = descr }
			    in
				case BF.exportPidOf bfc of
				    NONE => ()
				  | SOME pid =>
					(TextIO.output (s, " ");
					 TextIO.output (s, PS.toHex pid))
			    end
			    fun writePidLine s (p, set) =
				if StableSet.isEmpty set then ()
				else (TextIO.output (s, SrcPath.descr p);
				      StableSet.app (writePid s) set;
				      TextIO.output (s, "\n"))
			    fun writePidMap s =
				SrcPathMap.appi (writePidLine s) frontiers
			in
			    if deliver then
				(SafeIO.perform
				 { openIt = fn () =>
				       AutoDir.openTextOut listfile,
				   closeIt = TextIO.closeOut,
				   work = writeBootList,
				   cleanup = fn _ =>
				       OS.FileSys.remove listfile
				       handle _ => () };
				 SafeIO.perform
				 { openIt = fn () =>
				       AutoDir.openTextOut pidmapfile,
				   closeIt = TextIO.closeOut,
				   work = writePidMap,
				   cleanup = fn _ =>
				       OS.FileSys.remove pidmapfile
				       handle _ => () };
				 Say.say
				      ["New boot directory has been built.\n"])
			    else ();
			    true
			end
			else false
		    end
		in
		    SOME ((g, gp, pcmode), thunk)
		end
	end handle Option => (Compile.reset (); NONE)
	    	   (* to catch valOf failures in "rt" *)
    in
	case BuildInitDG.build ginfo_nocore initgspec of
	    SOME x => mk_main_compile x
	  | NONE => NONE
    end

    fun compile deliver dbopt =
	case mk_compile deliver NONE dbopt of
	    NONE => false
	  | SOME (_, thunk) => thunk ()

    local
	fun slave (dirbase, root) =
	    case mk_compile false (SOME root) (SOME dirbase) of
		NONE => NONE
	      | SOME ((g, gp, pcmode), _) => let
		    val trav = Compile.newSbnodeTraversal () gp
		    fun trav' sbn = isSome (trav sbn)
		in
		    SOME (g, trav', pcmode)
		end
    in
	val _ = CMBSlaveHook.init archos slave
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
			 cleanup = fn _ => () }
    fun deliver () = deliver' NONE
    val symval = SSV.symval
end
