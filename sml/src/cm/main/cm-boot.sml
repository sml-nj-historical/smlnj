(*
 * This is the module that actually puts together the contents of the
 * structure CM that people find in full-cm.cm.  A "minimal" structure
 * CM is defined in CmHook, but it needs to be initialized at bootstrap
 * time.
 *
 *   Copyright (c) 1999 by Lucent Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
functor LinkCM (structure HostMachDepVC : MACHDEP_VC) = struct

  datatype envrequest = AUTOLOAD | BARE

  local
      structure E = GenericVC.Environment
      structure DE = DynamicEnv
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
      val my_archos =
	  concat [HostMachDepVC.architecture, "-", FilenamePolicy.kind2name os]

      structure SSV =
	  SpecificSymValFn (structure MachDepVC = HostMachDepVC
			    val os = os)

      val emptydyn = E.dynamicPart E.emptyEnv
      val system_values = ref (SrcPathMap.empty: E.dynenv SrcPathMap.map)

      structure Compile =
	  CompileFn (structure MachDepVC = HostMachDepVC
		     val compile_there = Servers.compile o SrcPath.descr)

      structure BFC =
	  BfcFn (structure MachDepVC = HostMachDepVC)

      structure Link =
	  LinkFn (structure MachDepVC = HostMachDepVC
		  structure BFC = BFC
		  val system_values = system_values)

      structure AutoLoad = AutoLoadFn
	  (structure C = Compile
	   structure L = Link
	   structure BFC = BFC)

      val mkBootList = #l o MkBootList.group (fn p => p)

      fun init_servers (GroupGraph.GROUP { grouppath, ... }) =
	  Servers.cm { archos = my_archos, project = SrcPath.descr grouppath }

      fun recomp_runner gp g = let
	  val _ = init_servers g
	  fun store _ = ()
	  val { group, ... } = Compile.newTraversal (Link.evict, store, g)
      in
	  isSome (Servers.withServers (fn () => group gp))
	  before Link.cleanup gp
      end

      (* This function combines the actions of "recompile" and "exec".
       * When successful, it combines the results (thus forming a full
       * environment) and adds it to the toplevel environment. *)
      fun make_runner add_bindings gp g = let
	  val { store, get } = BFC.new ()
	  val _ = init_servers g
	  val { group = c_group, ... } =
	      Compile.newTraversal (Link.evict, store, g)
	  val { group = l_group, ... } = Link.newTraversal (g, get)
	  val GroupGraph.GROUP { required = rq, ... } = g
      in
	  case Servers.withServers (fn () => c_group gp) of
	      NONE => false
	    | SOME { stat, sym} =>
		  (* Before executing the code, we announce the privileges
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
		     | SOME dyn =>
			   (if add_bindings then
				let val delta = E.mkenv { static = stat,
							  symbolic = sym,
							  dynamic = dyn }
				    val base = #get ER.topLevel ()
				    val new =
					BE.concatEnv (CoerceEnv.e2b delta,
						      base)
				in
				    #set ER.topLevel new;
				    Say.vsay ["[New bindings added.]\n"]
				end
			    else ();
			    true))
      end

      val al_greg = GroupReg.new ()

      (* Instantiate the stabilization mechanism. *)
      structure Stabilize =
	  StabilizeFn (structure MachDepVC = HostMachDepVC
		       fun recomp gp g = let
			   val { store, get } = BFC.new ()
			   val _ = init_servers g
			   val { group, ... } =
			       Compile.newTraversal (Link.evict, store, g)
		       in
			   case Servers.withServers (fn () => group gp) of
			       NONE => NONE
			     | SOME _ => SOME get
		       end
		       val getII = Compile.getII)

      (* Access to the stabilization mechanism is integrated into the
       * parser. I'm not sure if this is the cleanest way, but it works
       * well enough. *)
      structure Parse = ParseFn (structure Stabilize = Stabilize
				 fun evictStale () =
				     (Compile.evictStale ();
				      Link.evictStale ())
				 val pending = AutoLoad.getPending)

      local
	  type kernelValues =
	      {	corenv : BE.environment,
		init_group : GroupGraph.group }

	  val fnpolicy = FilenamePolicy.colocate
	      { os = os, arch = HostMachDepVC.architecture }

	  val pcmode = PathConfig.new ()

	  val theValues = ref (NONE: kernelValues option)

      in
	  (* cancelling anchors cannot affect the order of existing paths
	   * (it may invalidate some paths; but all other ones stay as
	   * they are) *)
	  fun setAnchor a NONE = PathConfig.cancel (pcmode, a)
	    | setAnchor a (SOME s) = (PathConfig.set (pcmode, a, s);
				      SrcPath.sync ())
	  (* same goes for reset because it just cancels all anchors... *)
	  fun resetPathConfig () = PathConfig.reset pcmode
	  (* get the current binding for an anchor *)
	  fun getAnchor a () =
	      Option.map (fn f => f ()) (PathConfig.configAnchor pcmode a)

	  fun mkStdSrcPath s =
	      SrcPath.standard pcmode { context = SrcPath.cwdContext (),
				        spec = s }

	  fun getPending () = let
	      fun one (s, _) = let
		  val nss = Symbol.nameSpaceToString (Symbol.nameSpace s)
		  val n = Symbol.name s
	      in
		  concat ["  ", nss, " ", n, "\n"]
	      end
	  in
	      map one (SymbolMap.listItemsi (AutoLoad.getPending ()))
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

	  fun getTheValues () = valOf (!theValues)
	      handle Option => raise Fail "CMBoot: theParam not initialized"

	  fun param () = let
	      val v = getTheValues ()
	  in
	      { fnpolicy = fnpolicy,
		pcmode = pcmode,
		symval = SSV.symval,
		keep_going = #get StdConfig.keep_going (),
		corenv = #corenv v }
	  end

	  val init_group = #init_group o getTheValues

	  fun dropPickles () =
	      if #get StdConfig.conserve_memory () then
		  Parse.dropPickles ()
	      else ()

	  fun parse_arg (gr, sflag, p) =
	      { load_plugin = load_plugin, gr = gr, param = param (),
	        stabflag = sflag, group = p,
		init_group = init_group (), paranoid = false }

	  and autoload s = let
	      val p = mkStdSrcPath s
	  in
	      (case Parse.parse (parse_arg (al_greg, NONE, p)) of
		   NONE => false
		 | SOME (g, _) =>
		   (AutoLoad.register (GenericVC.EnvRef.topLevel, g);
		    true))
	      before dropPickles ()
	  end

	  and run mkSrcPath sflag f s = let
	      val p = mkSrcPath s
	      val gr = GroupReg.new ()
	  in
	      (case Parse.parse (parse_arg (gr, sflag, p)) of
		   NONE => false
		 | SOME (g, gp) => f gp g)
	      before dropPickles ()
	  end

	  and load_plugin context x = let
	      val _ = Say.vsay ["[attempting to load plugin ", x, "]\n"]
	      fun mkSrcPath s =
		  SrcPath.standard pcmode { context = context, spec = s }
	      val success =
		  run mkSrcPath NONE (make_runner false) x handle _ => false
	  in
	      if success then
		  Say.vsay ["[plugin ", x, " loaded successfully]\n"]
	      else
		  Say.vsay ["[unable to load plugin ", x, "]\n"];
	      success
	  end

	  fun cwd_load_plugin x = load_plugin (SrcPath.cwdContext ()) x

	  fun stabilize_runner gp g = true

	  fun stabilize recursively =
	      run mkStdSrcPath (SOME recursively) stabilize_runner
	  val recomp = run mkStdSrcPath NONE recomp_runner
	  val make = run mkStdSrcPath NONE (make_runner true)

	  (* I would have liked to express this using "run", but "run"
	   * thinks it has to return a bool... *)
	  fun mk_standalone sflag s = let
	      val p = mkStdSrcPath s
	      val gr = GroupReg.new ()
	  in
	      (case Parse.parse (parse_arg (gr, sflag, p)) of
		   NONE => NONE
		 | SOME (g, gp) =>
		   if isSome sflag orelse recomp_runner gp g then
		       SOME (mkBootList g)
		   else NONE)
	      before dropPickles ()
	  end

	  fun slave () = let
	      val gr = GroupReg.new ()
	      fun parse p = Parse.parse (parse_arg (gr, NONE, p))
	  in
	      Slave.slave { pcmode = pcmode,
			    parse = parse,
			    my_archos = my_archos,
			    sbtrav = Compile.newSbnodeTraversal,
			    make = make }
	  end

	  fun al_ginfo () = { param = param (),
			      groupreg = al_greg,
			      errcons = EM.defaultConsumer () }

	  val al_manager =
	      AutoLoad.mkManager { get_ginfo = al_ginfo,
				   dropPickles = dropPickles }

	  fun al_manager' (ast, _, ter) = al_manager (ast, ter)

	  fun reset () =
	      (Compile.reset ();
	       Link.reset ();
	       AutoLoad.reset ();
	       Parse.reset ();
	       SmlInfo.reset ())

	  fun initTheValues (bootdir, de, er, autoload_postprocess) = let
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
			cleanup = fn _ => () }
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

	      val pidmapfile = P.concat (bootdir, BtNames.pidmap)
	      fun readpidmap s = let
		  fun loop m = let
		      fun enter (d, pids) = let
			  fun enter1 (hexp, e) =
			      case GenericVC.PersStamps.fromHex hexp of
				  SOME p => (DE.bind (p, DE.look de p, e)
					     handle DE.Unbound => e)
				| NONE => e
		      in
			  SrcPathMap.insert (m, SrcPath.fromDescr pcmode d,
					     foldl enter1 emptydyn pids)
		      end
		  in
		      case TextIO.inputLine s of
			  "" => m
			| line => (case String.tokens Char.isSpace line of
				       d :: pids => loop (enter (d, pids))
				     | _ => loop m)
		  end
	      in
		  system_values := loop SrcPathMap.empty
	      end
	      
	      val _ =
		  SafeIO.perform { openIt = fn () => TextIO.openIn pidmapfile,
				   closeIt = TextIO.closeIn,
				   work = readpidmap,
				   cleanup = fn _ => () }

	      val initgspec = mkStdSrcPath BtNames.initgspec
	      val ginfo = { param = { fnpolicy = fnpolicy,
				      pcmode = pcmode,
				      symval = SSV.symval,
				      keep_going = false,
				      corenv = BE.emptyEnv },
			    groupreg = GroupReg.new (),
			    errcons = EM.defaultConsumer () }
	      fun loadInitGroup () =
		  Stabilize.loadStable ginfo
		    { getGroup = fn _ => raise Fail "CMBoot: initial getGroup",
		      anyerrors = ref false }
		    initgspec
	  in
	      case loadInitGroup () of
		  NONE => raise Fail "CMBoot: unable to load init group"
		| SOME init_group => let
		      val _ = Compile.reset ()
		      val _ = Link.reset ()

		      val { exports = ctm, ... } =
			  Compile.newTraversal (fn _ => fn _ => (),
						fn _ => (),
						init_group)
		      val { exports = ltm, ... } = Link.newTraversal
			  (init_group, fn _ => raise Fail "init: get bfc?")

		      fun getSymTrav (tr_m, sy) =
			  case SymbolMap.find (tr_m, sy) of
			      NONE => raise Fail "init: bogus init group (1)"
			    | SOME tr => tr

		      val core_ct = getSymTrav (ctm, PervCoreAccess.coreStrSym)
		      val core_lt = getSymTrav (ltm, PervCoreAccess.coreStrSym)
		      val perv_ct = getSymTrav (ctm, PervCoreAccess.pervStrSym)
		      val perv_lt = getSymTrav (ltm, PervCoreAccess.pervStrSym)

		      fun doTrav t =
			  case t ginfo of
			      SOME r => r
			    | NONE => raise Fail "init: bogus init group (2)"

		      val { stat = corestat, sym = coresym } = doTrav core_ct
		      val coredyn = doTrav core_lt
		      val { stat = pervstat, sym = pervsym } = doTrav perv_ct
		      val pervdyn = doTrav perv_lt

		      val corenv =
			  BE.mkenv { static = CoerceEnv.es2bs corestat,
				     symbolic = coresym,
				     dynamic = coredyn }
		      val core_symdyn =
			  E.mkenv { static = E.staticPart E.emptyEnv,
				    dynamic = coredyn, symbolic = coresym }

		      val pervasive = E.mkenv { static = pervstat,
					        symbolic = pervsym,
						dynamic = pervdyn }

		      fun bare_autoload x =
			  (Say.say
			    ["!* ", x,
			     ": \"autoload\" not available, using \"make\"\n"];
			   make x)
		      val bare_preload =
			  Preload.preload { make = make,
					    autoload = bare_autoload }
		      val standard_preload =
			  Preload.preload { make = make, autoload = autoload }
		  in
		      #set ER.core (BE.staticPart corenv);
		      #set ER.pervasive (E.layerEnv (pervasive, core_symdyn));
		      #set ER.topLevel BE.emptyEnv;
		      theValues := SOME { corenv = corenv,
					  init_group = init_group };
		      case er of
			  BARE =>
			      (bare_preload BtNames.bare_preloads;
			       system_values := SrcPathMap.empty;
			       NONE)
			| AUTOLOAD =>
			      (HostMachDepVC.Interact.installCompManager
			            (SOME al_manager');
				    standard_preload BtNames.standard_preloads;
			       CmHook.init
			         { stabilize = stabilize,
				   recomp = recomp,
				   make = make,
				   autoload = autoload };
			       (* unconditionally drop all library pickles *)
			       Parse.dropPickles ();
			       SOME (autoload_postprocess ()))
		  end
	  end
      end
  in
    fun init (bootdir, de, er) = let
	fun procCmdLine () = let
	    val autoload = ignore o autoload
	    val make = ignore o make
	    fun p (f, ("sml" | "sig"), mk) = HostMachDepVC.Interact.useFile f
	      | p (f, "cm", mk) = mk f
	      | p (f, e, mk) = Say.say ["!* unable to process `", f,
					"' (unknown extension `", e, "')\n"]
	    fun arg ("-a", _) = autoload
	      | arg ("-m", _) = make
	      | arg (f, mk) =
		(p (f,
		    String.map Char.toLower (getOpt (OS.Path.ext f, "<none>")),
		    mk);
		 mk)
	in
	    case SMLofNJ.getArgs () of
		["@CMslave"] => (#set StdConfig.verbose false; slave ())
	      | l => ignore (foldl arg autoload l)
	end
    in
	initTheValues (bootdir, de, er,
		       fn () => (Cleanup.install initPaths;
				 procCmdLine))
    end

    structure CM :> CM = struct
	type 'a controller = { get : unit -> 'a, set : 'a -> unit }

	structure Anchor = struct
	    fun anchor a = { get = getAnchor a, set = setAnchor a }
	    val reset = resetPathConfig
	end

	structure Control = struct
	    val keep_going = StdConfig.keep_going
	    val verbose = StdConfig.verbose
	    val parse_caching = StdConfig.parse_caching
	    val warn_obsolete = StdConfig.warn_obsolete
	    val debug = StdConfig.debug
	    val conserve_memory = StdConfig.conserve_memory
	end

	structure Library = struct
	    type lib = SrcPath.t
	    val known = Parse.listLibs
	    val descr = SrcPath.descr
	    val osstring = SrcPath.osstring
	    val dismiss = Parse.dismissLib
	end

	structure State = struct
	    val synchronize = SrcPath.sync
	    val reset = reset
	    val pending = getPending
	end

	structure Server = struct
	    type server = Servers.server
	    fun start x = Servers.start x before SrcPath.invalidateCwd ()
	    val stop = Servers.stop
	    val kill = Servers.kill
	    val name = Servers.name
	end

	val autoload = autoload
	val make = make
	val recomp = recomp
	val stabilize = stabilize

	val symval = SSV.symval
	val load_plugin = cwd_load_plugin 
	val mk_standalone = mk_standalone
    end

    structure Tools = ToolsFn (val load_plugin = cwd_load_plugin
			       val mkStdSrcPath = mkStdSrcPath)

    val load_plugin = load_plugin
  end
end
