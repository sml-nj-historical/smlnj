(*
 * This is the module that actually puts together the contents of the
 * structure CM that people find at the top-level.  The "real" structure
 * CM is defined in CmHook, but it needs to be initialized at bootstrap
 * time -- and _that_ is what's done here.
 *
 *   Copyright (c) 1999 by Lucent Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
functor LinkCM (structure HostMachDepVC : MACHDEP_VC) = struct

  datatype envrequest = AUTOLOAD | BARE

  local
      structure YaccTool = YaccTool
      structure LexTool = LexTool
      structure BurgTool = BurgTool

      structure E = GenericVC.Environment
      structure SE = GenericVC.StaticEnv
      structure ER = GenericVC.EnvRef
      structure BE = GenericVC.BareEnvironment
      structure CMSE = GenericVC.CMStaticEnv
      structure S = GenericVC.Symbol
      structure CoerceEnv = GenericVC.CoerceEnv
      structure EM = GenericVC.ErrorMsg
      structure BF = HostMachDepVC.Binfile
      structure P = OS.Path
      structure F = OS.FileSys
      structure DG = DependencyGraph

      val os = SMLofNJ.SysInfo.getOSKind ()

      structure SSV =
	  SpecificSymValFn (structure MachDepVC = HostMachDepVC
			    val os = os)

      val emptydyn = E.dynamicPart E.emptyEnv
      val system_values = ref emptydyn

      structure Compile =
	  CompileFn (structure MachDepVC = HostMachDepVC
		     val compile_there = Servers.compile)

      structure Link =
	  LinkFn (structure MachDepVC = HostMachDepVC
		  val system_values = system_values)

      structure BFC =
	  BfcFn (structure MachDepVC = HostMachDepVC)

      structure AutoLoad = AutoLoadFn
	  (structure C = Compile
	   structure L = Link
	   structure BFC = BFC)

      fun recomp_runner gp g = let
	  fun store _ = ()
	  val { group, ... } = Compile.newTraversal (Link.evict, store, g)
      in
	  isSome (Servers.withServers (fn () => group gp))
	  before Link.cleanup gp
      end

      (* This function combines the actions of "recompile" and "exec".
       * When successful, it combines the results (thus forming a full
       * environment) and adds it to the toplevel environment. *)
      fun make_runner gp g = let
	  val { store, get } = BFC.new ()
	  val { group = c_group, ... } =
	      Compile.newTraversal (Link.evict, store, g)
	  val { group = l_group, ... } = Link.newTraversal (g, get)
	  val GroupGraph.GROUP { required = rq, ... } = g
      in
	  case Servers.withServers (fn () => c_group gp) of
	      NONE => false
	    | SOME { stat, sym} =>
		  (* Before executing the code, we announce the priviliges
		   * that are being invoked.  (For the time being, we assume
		   * that everybody has every conceivable privilege, but at
		   * the very least we announce which ones are being made
		   * use of.) *)
		  (Link.cleanup gp;
		   if StringSet.isEmpty rq then ()
		   else Say.say ("$Execute: required privileges are:\n" ::
		     map (fn s => ("  " ^ s ^ "\n")) (StringSet.listItems rq));
		   case l_group gp of
		       NONE => false
		     | SOME dyn => let
			   val delta = E.mkenv { static = stat, symbolic = sym,
						 dynamic = dyn }
			   val base = #get ER.topLevel ()
			   val new = BE.concatEnv (CoerceEnv.e2b delta, base)
		       in
			   #set ER.topLevel new;
			   Say.vsay ["[New bindings added.]\n"];
			   true
		       end)
      end

      val al_greg = GroupReg.new ()

      (* Instantiate the stabilization mechanism. *)
      structure Stabilize =
	  StabilizeFn (structure MachDepVC = HostMachDepVC
		       fun recomp gp g = let
			   val GroupGraph.GROUP { grouppath, ... } = g
			   val { store, get } = BFC.new ()
			   val { group, ... } =
			       Compile.newTraversal (Link.evict, store, g)
		       in
			   case Servers.withServers (fn () => group gp) of
			       NONE => NONE
			     | SOME _ => SOME get
		       end
		       fun destroy_state gp i =
			   (Compile.evict i;
			    Link.evict gp i)
		       val getII = Compile.getII)

      (* Access to the stabilization mechanism is integrated into the
       * parser. I'm not sure if this is the cleanest way, but it works
       * well enough. *)
      structure Parse = ParseFn (structure Stabilize = Stabilize
				 val pending = AutoLoad.getPending)

      local
	  type kernelValues =
	      { primconf : Primitive.configuration,
	        pervasive : E.environment,
		corenv : BE.staticEnv,
		pervcorepids : PidSet.set }

	  val fnpolicy = FilenamePolicy.colocate
	      { os = os, arch = HostMachDepVC.architecture }

	  val pcmode = PathConfig.new ()

	  val theValues = ref (NONE: kernelValues option)

      in
	  fun setAnchor (a, s) =
	      (PathConfig.set (pcmode, a, s); SrcPath.sync ())
	  (* cancelling anchors cannot affect the order of existing paths
	   * (it may invalidate some paths; but all other ones stay as
	   * they are) *)
	  fun cancelAnchor a = PathConfig.cancel (pcmode, a)
	  (* same goes for reset because it just cancels all anchors... *)
	  fun resetPathConfig () = PathConfig.reset pcmode

	  fun showPending () = let
	      fun one (s, _) = let
		  val nss = Symbol.nameSpaceToString (Symbol.nameSpace s)
		  val n = Symbol.name s
	      in
		  Say.say ["  ", nss, " ", n, "\n"]
	      end
	  in
	      SymbolMap.appi one (AutoLoad.getPending ())
	  end

	  fun initPaths () = let
	      val lpcth = #get StdConfig.local_pathconfig ()
	      val p = case lpcth () of
		  NONE => []
		| SOME f => [f]
	      val p = #get StdConfig.pathcfgspec () :: p
	      fun processOne f = PathConfig.processSpecFile (pcmode, f)
		  handle _ => ()
	  in
	      app processOne p
	  end

	  fun param () = let
	      val v = valOf (!theValues)
		  handle Option =>
		      raise Fail "CMBoot: theParam not initialized"
	  in
	      { primconf = #primconf v,
	        fnpolicy = fnpolicy,
		pcmode = pcmode,
		symval = SSV.symval,
		keep_going = #get StdConfig.keep_going (),
		pervasive = #pervasive v,
		corenv = #corenv v,
		pervcorepids = #pervcorepids v }
	  end

	  fun autoload s = let
	      val c = SrcPath.cwdContext ()
	      val p = SrcPath.standard pcmode { context = c, spec = s }
	  in
	      case Parse.parse (SOME al_greg) (param ()) NONE p of
		  NONE => false
		| SOME (g, _) =>
		      (AutoLoad.register (GenericVC.EnvRef.topLevel, g);
		       true)
	  end

	  fun al_ginfo () = { param = param (),
			      groupreg = al_greg,
			      errcons = EM.defaultConsumer () }

	  val al_manager = AutoLoad.mkManager al_ginfo

	  fun al_manager' (ast, _, ter) = al_manager (ast, ter)

	  fun run sflag f s = let
	      val c = SrcPath.cwdContext ()
	      val p = SrcPath.standard pcmode { context = c, spec = s }
	  in
	      Servers.cm p;
	      case Parse.parse NONE (param ()) sflag p of
		  NONE => false
		| SOME (g, gp) =>
		      SafeIO.perform { openIt = fn () => (),
				       closeIt = Servers.reset,
				       work = fn () => f gp g,
				       cleanup = fn () => () }
	  end

	  val listLibs = Parse.listLibs
	  fun dismissLib l = let
	      val c = SrcPath.cwdContext ()
	      val p = SrcPath.standard pcmode { context = c, spec = l }
	  in
	      Parse.dismissLib p
	  end

	  fun stabilize_runner gp g = true

	  fun stabilize recursively = run (SOME recursively) stabilize_runner
	  val recomp = run NONE recomp_runner
	  val make = run NONE make_runner

	  fun slave () = let
	      fun shutdown () = OS.Process.exit OS.Process.success
	      fun say_ok () = Say.say ["SLAVE: ok\n"]
	      fun say_error () = Say.say ["SLAVE: error\n"]
	      fun say_pong () = Say.say ["SLAVE: pong\n"]

	      val touch = HostMachDepVC.Interact.useStream o TextIO.openString 
		  
	      fun waitForStart () = let
		  val line = TextIO.inputLine TextIO.stdIn
	      in
		  if line = "" then shutdown ()
		  else case String.tokens Char.isSpace line of
		      ["cm", d, f] => do_cm (d, f)
		    | ["cmb", archos, d, db] => do_cmb (archos, d, db)
		    | ["ping"] => (say_pong (); waitForStart ())
		    | ["finish"] => (say_ok (); waitForStart ())
		    | ["shutdown"] => shutdown ()
		    | _ => (say_error (); waitForStart ())
	      end handle _ => (say_error (); waitForStart ())

	      and do_cmb (archos, d, db) = let
		  val _ = OS.FileSys.chDir d
		  val c = SrcPath.cwdContext ()
		  val slave = CMBSlave.slave { load = autoload, touch = touch }
	      in
		  case slave archos db of
		      NONE => (say_error (); waitForStart ())
		    | SOME (g, trav) => let
			  val _ = say_ok ()
			  val index = Reachable.snodeMap g
		      in
			  workLoop (index, trav, c)
		      end
	      end handle _ => (say_error (); waitForStart ())

	      and do_cm (d, f) = let
		  val _ = OS.FileSys.chDir d
		  val c = SrcPath.cwdContext ()
		  val p = SrcPath.native { context = c, spec = f }
	      in
		  case Parse.parse NONE (param ()) NONE p of
		      NONE => (say_error (); waitForStart ())
		    | SOME (g, gp) => let
			  val _ = say_ok ()
			  val index = Reachable.snodeMap g
			  val trav = Compile.newSbnodeTraversal () gp
			  fun trav' sbn = isSome (trav sbn)
		      in
			  workLoop (index, trav', c)
		      end
	      end handle _ => (say_error (); waitForStart ())

	      and workLoop (index, trav, c) = let
		  fun f2sn f =
		      SrcPathMap.find (index,
				       SrcPath.native { context = c,
						        spec = f })
		  fun loop () = let
		      val line = TextIO.inputLine TextIO.stdIn
		  in
		      if line = "" then shutdown ()
		      else case String.tokens Char.isSpace line of
			  ["compile", f] => let
			      val p = SrcPath.native { context = c, spec = f }
			  in
			      case SrcPathMap.find (index, p) of
				  NONE => (say_error (); loop ())
				| SOME sn => let
				      val sbn = DG.SB_SNODE sn
				  in
				      if trav sbn then (say_ok (); loop ())
				      else (say_error (); loop ())
				  end handle _ => (say_error (); loop ())
			  end
			| ["cm", d, f] => do_cm (d, f)
			| ["cmb", archos, d, db] => do_cmb (archos, d, db)
			| ["finish"] => (say_ok (); waitForStart ())
			| ["ping"] => (say_pong (); loop ())
			| ["shutdown"] => shutdown ()
			| _ => (say_error (); loop ())
		  end handle _ => (say_error (); loop ())
	      in
		  loop ()
	      end
	  in
	      ignore (Signals.setHandler (Signals.sigINT, Signals.IGNORE));
	      say_ok ();		(* announce readiness *)
	      waitForStart () handle _ => ();
	      OS.Process.exit OS.Process.failure
	  end

	  fun reset () =
	      (Compile.reset ();
	       Link.reset ();
	       AutoLoad.reset ();
	       Parse.reset ();
	       SmlInfo.forgetAllBut SrcPathSet.empty)

	  fun initTheValues (bootdir, er) = let
	      val _ = let
		  fun listDir ds = let
		      fun loop l =
			  case F.readDir ds of
			      "" => l
			    | x => loop (x :: l)
		  in
		      loop []
		  end
		  val fileList = SafeIO.perform
		      { openIt = fn () => F.openDir bootdir,
		        closeIt = F.closeDir,
			work = listDir,
			cleanup = fn () => () }
		  fun isDir x = F.isDir x handle _ => false
		  fun subDir x = let
		      val d = P.concat (bootdir, x)
		  in
		      if isDir d then SOME (x, d) else NONE
		  end
		  val pairList = List.mapPartial subDir fileList
	      in
		  app (fn (x, d) => PathConfig.set (pcmode, x, d)) pairList
	      end
	      val initgspec =
		  SrcPath.standard pcmode { context = SrcPath.cwdContext (),
					    spec = BtNames.initgspec }
	      val ginfo = { param = { primconf = Primitive.primEnvConf,
				      fnpolicy = fnpolicy,
				      pcmode = pcmode,
				      symval = SSV.symval,
				      keep_going = false,
				      pervasive = E.emptyEnv,
				      corenv = BE.staticPart BE.emptyEnv,
				      pervcorepids = PidSet.empty },
			    groupreg = GroupReg.new (),
			    errcons = EM.defaultConsumer () }
	  in
	      case BuildInitDG.build ginfo initgspec of
		  NONE => raise Fail "CMBoot: BuiltInitDG.build"
		| SOME { rts, core, pervasive, primitives, ... } => let
		      (* It is absolutely crucial that we don't finish the
		       * recomp traversal until we are done with all
		       * nodes of the InitDG.  This is because we have
		       * been cheating, and if we ever have to try and
		       * fetch assembly.sig or core.sml in a separate
		       * traversal, it will fail. *)
		      val sbnode = Compile.newSbnodeTraversal ()
		      fun get n = let
			  val { ii, ctxt } = valOf (sbnode ginfo n)
			  val { statpid, statenv, symenv, sympid } = ii
			  (* We have not implemented the "sbnode" part
			   * in the Link module.
			   * But at boot time any relevant value should be
			   * available as a sysval, so there is no problem.
			   *
			   * WARNING!  HACK!
			   * We are cheating somewhat by taking advantage
			   * of the fact that the staticPid is always
			   * the same as the exportPid if the latter exists.
			   *)
			  val d = case Link.sysval (SOME statpid) of
			      SOME d => d
			    | NONE => emptydyn
			  val env = E.mkenv { static = statenv (),
					      symbolic = symenv (),
					      dynamic = d }
			  val pidInfo = { statpid = statpid,
					  sympid = sympid,
					  ctxt = ctxt }
		      in
			  (env, pidInfo)
		      end
		      fun getPspec (name, n) = let
			  val (env, pidInfo) = get n
		      in
			  { name = name, env = env, pidInfo = pidInfo }
		      end

		      val (core, corePidInfo) = get core
		      val corenv = CoerceEnv.es2bs (E.staticPart core)
		      val (rts, _) = get rts
		      val (pervasive0, pervPidInfo) = get pervasive
		      val pspecs = map getPspec primitives
		      val core_symdyn =
			  E.mkenv { static = E.staticPart E.emptyEnv,
				    dynamic = E.dynamicPart core,
				    symbolic = E.symbolicPart core }
		      val pervasive = E.layerEnv (pervasive0, core_symdyn)
		      val pervcorepids =
			  PidSet.addList (PidSet.empty,
					  [#statpid corePidInfo,
					   #statpid pervPidInfo,
					   #sympid pervPidInfo])
		  in
		      Compile.reset ();
		      Link.reset ();
		      #set ER.core corenv;
		      #set ER.pervasive pervasive;
		      #set ER.topLevel BE.emptyEnv;
		      theValues :=
		        SOME { primconf = Primitive.configuration pspecs,
			       pervasive = pervasive,
			       corenv = corenv,
			       pervcorepids = pervcorepids };
		      case er of
			  BARE =>
			      (make "basis.cm";
			       make "host-compiler.cm";
			       system_values := emptydyn)
			| AUTOLOAD =>
			      (HostMachDepVC.Interact.installCompManager
			            (SOME al_manager');
			       autoload "basis.cm";
			       autoload "host-cm.cm";
			       CmHook.init
			         { stabilize = stabilize,
				   recomp = recomp,
				   make = make,
				   autoload = autoload,
				   reset = reset,
				   verbose = StdConfig.verbose,
				   debug = StdConfig.debug,
				   keep_going = StdConfig.keep_going,
				   warn_obsolete = StdConfig.warn_obsolete,
				   parse_caching = StdConfig.parse_caching,
				   setAnchor = setAnchor,
				   cancelAnchor = cancelAnchor,
				   resetPathConfig = resetPathConfig,
				   synchronize = SrcPath.sync,
				   showPending = showPending,
				   listLibs = listLibs,
				   dismissLib = dismissLib,
				   symval = SSV.symval,
				   server_start = Servers.start,
				   server_stop = Servers.stop,
				   server_kill = Servers.kill })

		  end
	  end
      end
  in
    fun init (bootdir, de, er) =
	(system_values := de;
	 initTheValues (bootdir, er);
	 Cleanup.install initPaths)

    fun procCmdLine () = let
	fun p (f, "sml") = HostMachDepVC.Interact.useFile f
	  | p (f, "sig") = HostMachDepVC.Interact.useFile f
	  | p (f, "cm") = ignore (make f)
	  | p (f, e) =
		(print (concat ["!* unable to process `", f,
				"' (unknown extension `", e, "')\n"]))
	fun c f = (f, String.map Char.toLower
		          (getOpt (OS.Path.ext f, "<none>")))
    in
	case SMLofNJ.getArgs () of
	    ["@CMslave"] => (#set StdConfig.verbose false; slave ())
	  | l => app (p o c) l
    end
  end
end
