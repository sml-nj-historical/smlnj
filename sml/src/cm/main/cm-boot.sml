(*
 * This is the module that actually puts together the contents of the
 * structure CM that people find in $smlnj/cm/full.cm.
 *
 *   Copyright (c) 1999, 2000 by Lucent Bell Laboratories
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
      structure S = GenericVC.Symbol
      structure EM = GenericVC.ErrorMsg
      structure BF = HostMachDepVC.Binfile
      structure P = OS.Path
      structure F = OS.FileSys
      structure DG = DependencyGraph
      structure GG = GroupGraph
      structure IM = IntMap

      val os = SMLofNJ.SysInfo.getOSKind ()
      val my_archos =
	  concat [HostMachDepVC.architecture, "-", FilenamePolicy.kind2name os]

      structure SSV =
	  SpecificSymValFn (structure MachDepVC = HostMachDepVC
			    val os = os)

      val emptydyn = E.dynamicPart E.emptyEnv
      val system_values =
	  ref (SrcPathMap.empty: E.dynenv IntMap.map SrcPathMap.map)

      structure StabModmap = StabModmapFn ()

      structure Compile =
	  CompileFn (structure MachDepVC = HostMachDepVC
		     structure StabModmap = StabModmap
		     val useStream = HostMachDepVC.Interact.useStream
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

      fun init_servers (GG.GROUP { grouppath, ... }) =
	  Servers.cm { archos = my_archos, project = SrcPath.descr grouppath }
	| init_servers GG.ERRORGROUP = ()

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
      fun make_runner _ _ GG.ERRORGROUP = false
	| make_runner add_bindings gp (g as GG.GROUP grec) = let
	      val { required = rq, ... } = grec
	      val { store, get } = BFC.new ()
	      val _ = init_servers g
	      val { group = c_group, ... } =
		  Compile.newTraversal (Link.evict, store, g)
	      val { group = l_group, ... } = Link.newTraversal (g, get)
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
				    val new = E.concatEnv (delta, base)
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
		       structure StabModmap = StabModmap
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
				 structure StabModmap = StabModmap
				 fun evictStale () =
				     (Compile.evictStale ();
				      Link.evictStale ())
				 val pending = AutoLoad.getPending)

      local
	  type kernelValues = { init_group : GG.group }

	  val fnpolicy = FilenamePolicy.colocate
	      { os = os, arch = HostMachDepVC.architecture }

	  val theValues = ref (NONE: kernelValues option)

      in
          val penv = SrcPath.newEnv ()

	  (* cancelling anchors cannot affect the order of existing paths
	   * (it may invalidate some paths; but all other ones stay as
	   * they are) *)
	  fun setAnchor a v = SrcPath.set_anchor (penv, a, v)
	  (* same goes for reset because it just cancels all anchors... *)
	  fun resetPathConfig () = SrcPath.reset_anchors penv
	  (* get the current binding for an anchor *)
	  fun getAnchor a () = SrcPath.get_anchor (penv, a)

	  fun mkStdSrcPath s =
	      SrcPath.file
	        (SrcPath.standard { err = fn s => raise Fail s, env = penv }
				  { context = SrcPath.cwd (), spec = s })

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
	      fun processOne f = let
		  val work =  SrcPath.processSpecFile
				  { env = penv, specfile = f, say = Say.say }
	      in
		  SafeIO.perform { openIt = fn () => TextIO.openIn f,
				   closeIt = TextIO.closeIn,
				   work = work,
				   cleanup = fn _ => () }
	      end handle _ => ()
	  in
	      app processOne p;
	      SrcPath.sync ()
	  end

	  fun getTheValues () = valOf (!theValues)
	      handle Option => raise Fail "CMBoot: theParam not initialized"

	  fun param () = let
	      val v = getTheValues ()
	  in
	      { fnpolicy = fnpolicy,
		penv = penv,
		symval = SSV.symval,
		keep_going = #get StdConfig.keep_going () }
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

	  and load_plugin' p = let
	      val d = SrcPath.descr p
	      val _ = Say.vsay ["[attempting to load plugin ", d, "]\n"]
	      val gr = GroupReg.new ()
	      val success =
		  ((case Parse.parse (parse_arg (gr, NONE, p)) of
			NONE => false
		      | SOME (g, gp) => make_runner false gp g)
		   before dropPickles ())
		  handle _ => false
	  in
	      if success then
		  Say.vsay ["[plugin ", d, " loaded successfully]\n"]
	      else
		  Say.vsay ["[unable to load plugin ", d, "]\n"];
	      success
	  end

	  and load_plugin context s = let
	      fun badname s = Say.say ["[bad plugin name: ", s, "]\n"]
	      val pp = SrcPath.standard { env = penv, err = badname }
					{ context = context, spec = s }
	  in
	      load_plugin' (SrcPath.file pp)
	  end

	  fun cwd_load_plugin x = load_plugin (SrcPath.cwd ()) x

	  fun stabilize_runner gp g = true

	  fun stabilize recursively =
	      run mkStdSrcPath (SOME recursively) stabilize_runner
	  val recomp = run mkStdSrcPath NONE recomp_runner
	  val make = run mkStdSrcPath NONE (make_runner true)


	  fun sources archos group = let
	      val policy =
		  case archos of
		      NONE => fnpolicy
		    | SOME ao => FilenamePolicy.colocate_generic ao
	      fun sourcesOf ((p, gth, _), (v, a)) =
		  if SrcPathSet.member (v, p) then (v, a)
		  else
		      let val v = SrcPathSet.add (v, p)
		      in case gth () of
			     GG.ERRORGROUP => (v, a)
			   | GG.GROUP { kind, sources, ... } => let
				 fun add (p, x, a) =
				     StringMap.insert
					 (a, SrcPath.osstring p, x)
				 val a = SrcPathMap.foldli add a sources
				 fun sg subgroups =
				     foldl sourcesOf (v, a) subgroups
			     in
				 case kind of
				     GG.LIB { kind, version } =>
				     (case kind of
					  GG.STABLE _ => let
					      val file = SrcPath.osstring p
					      val (a, x) =
						  StringMap.remove (a, file)
					      val sfile =
						  FilenamePolicy.mkStableName
						      policy (p, version)
					  in
					      (v,
					       StringMap.insert (a, sfile, x))
					  end
					| GG.DEVELOPED d => sg (#subgroups d))
				   | GG.NOLIB n => sg (#subgroups n)
			     end
		      end
	      val p = mkStdSrcPath group
	      val gr = GroupReg.new ()
	  in
	      (case Parse.parse (parse_arg (gr, NONE, p)) of
		   SOME (g, _) => let
		       val (_, sm) =
			   sourcesOf ((p, fn () => g, []),
				      (SrcPathSet.empty,
				       StringMap.singleton
					   (SrcPath.osstring p,
					    { class = "cm",
					      derived = false })))
		       fun add (s, { class, derived }, l) =
			   { file = s, class = class, derived = derived } :: l
		   in
		       SOME (StringMap.foldli add [] sm)
		   end
		 | _ => NONE)
	      before dropPickles ()
	  end

	  fun mk_standalone sflag { project, wrapper, target } = let
	      val hsfx = SMLofNJ.SysInfo.getHeapSuffix ()
	      fun extendTarget () =
		  OS.Path.joinBaseExt { base = target, ext = SOME hsfx }
	      val target =
		  case OS.Path.splitBaseExt target of
		      { base, ext = NONE } => extendTarget ()
		    | { base, ext = SOME e } =>
		      if e = hsfx then target else extendTarget ()
	      val pp = mkStdSrcPath project
	      val wp = mkStdSrcPath wrapper
	      val ts = TStamp.fmodTime target
	      val gr = GroupReg.new ()
	      fun do_wrapper () =
		  case Parse.parse (parse_arg (gr, NONE, wp)) of
		      NONE => NONE
		    | SOME (g, gp) =>
		      if recomp_runner gp g then SOME (mkBootList g)
		      else NONE
	  in
	      (case Parse.parse (parse_arg (gr, sflag, pp)) of
		   NONE => NONE
		 | SOME (g, gp) =>
		   if isSome sflag orelse recomp_runner gp g then
		       case (ts, !(#youngest gp)) of
			   (TStamp.TSTAMP tgt_t, TStamp.TSTAMP src_t) =>
			   if Time.< (tgt_t, src_t) then do_wrapper ()
			   else SOME []
			 | _ => do_wrapper ()
		   else NONE)
	      before dropPickles ()
	  end

	  fun slave () = let
	      val gr = GroupReg.new ()
	      fun parse p = Parse.parse (parse_arg (gr, NONE, p))
	  in
	      Slave.slave { penv = penv,
			    parse = parse,
			    my_archos = my_archos,
			    sbtrav = Compile.newSbnodeTraversal,
			    make = make }
	  end

	  (* This function works on behalf of the ml-build script.
	   * Having it here avoids certain startup-costs and also
	   * keeps ML code together.  (It used to be part of the
	   * script, but that proved difficult to maintain.) *)
	  fun mlbuild buildargs =
	      OS.Process.exit
	      (case buildargs of
		   [root, cmfile, heap, listfile, link] =>
		   (case mk_standalone NONE { project = root,
					      wrapper = cmfile,
					      target = heap } of
			NONE => (Say.say ["Compilation failed.\n"];
				 OS.Process.failure)
		     | SOME [] => (Say.say ["Heap was already up-to-date.\n"];
				   OS.Process.success)
		     | SOME l => let
			   val s = TextIO.openOut listfile
			   fun wr str = TextIO.output (s, str ^ "\n")
			   val n = length l
			   fun maxsz (s, n) = Int.max (size s, n)
			   val m = foldl maxsz 0 l
		       in
			   wr (concat ["%", Int.toString n, " ",
				       Int.toString m]);
			   app wr l;
			   TextIO.closeOut s;
			   OS.Process.system (concat [link,
						      " @SMLboot=", listfile])
		       end
		       handle _ => OS.Process.failure)
		 | _ => (Say.say ["bad arguments to @CMbuild\n"];
			 OS.Process.failure))

	  fun al_ginfo () = { param = param (),
			      groupreg = al_greg,
			      errcons = EM.defaultConsumer (),
			      youngest = ref TStamp.ancient }

	  val al_manager =
	      AutoLoad.mkManager { get_ginfo = al_ginfo,
				   dropPickles = dropPickles }

	  fun al_manager' (ast, _, ter) = al_manager (ast, ter)

	  fun reset () =
	      (Compile.reset ();
	       Link.reset ();
	       AutoLoad.reset ();
	       Parse.reset ();
	       SmlInfo.reset ();
	       StabModmap.reset ())

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
		  fun sa (x, d) = SrcPath.set_anchor (penv, x, SOME d)
	      in
		  app sa pairList
	      end

	      val pidmapfile = P.concat (bootdir, BtNames.pidmap)
	      fun readpidmap s = let
		  fun loop m = let
		      fun enter (d, pids) = let
			  fun enter1 (spec, pm) = let
			      val fromHex = GenericVC.PersStamps.fromHex
			  in
			      case String.tokens (fn c => c = #":") spec of
				  [pos, hexp] =>
				  (case (fromHex hexp, Int.fromString pos) of
				       (SOME p, SOME i) =>
				       (IM.insert (pm, i,
						   DE.bind (p, DE.look de p,
							    emptydyn))
					handle DE.Unbound => pm)
				     | _ => pm)
				| _ => pm
			  end
		      in
			  SrcPathMap.insert (m, SrcPath.decode penv d,
					     foldl enter1 IM.empty pids)
		      end
		  in
		      case TextIO.inputLine s of
			  "" => m
			| line => (case String.tokens Char.isSpace line of
				       d :: pids => loop (enter (d, pids))
				     | _ => loop m)
		  end
		  val m = loop SrcPathMap.empty
	      in
		  system_values := m
	      end
	      
	      val _ =
		  SafeIO.perform { openIt = fn () => TextIO.openIn pidmapfile,
				   closeIt = TextIO.closeIn,
				   work = readpidmap,
				   cleanup = fn _ => () }

	      val initgspec = mkStdSrcPath BtNames.initgspec
	      val ginfo = { param = { fnpolicy = fnpolicy,
				      penv = penv,
				      symval = SSV.symval,
				      keep_going = false },
			    groupreg = GroupReg.new (),
			    errcons = EM.defaultConsumer (),
			    youngest = ref TStamp.ancient }
	      fun loadInitGroup () =
		  Stabilize.loadStable
		      { getGroup = fn _ =>
				      raise Fail "CMBoot: initial getGroup",
			anyerrors = ref false }
		      (ginfo, initgspec, NONE, [])
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

		      val perv_ct = getSymTrav (ctm, PervAccess.pervStrSym)
		      val perv_lt = getSymTrav (ltm, PervAccess.pervStrSym)

		      fun doTrav t =
			  case t ginfo of
			      SOME r => r
			    | NONE => raise Fail "init: bogus init group (2)"

		      val { stat = pervstat, sym = pervsym } = doTrav perv_ct
		      val pervdyn = doTrav perv_lt

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
		      #set ER.pervasive pervasive;
		      #set ER.topLevel E.emptyEnv;
		      theValues := SOME { init_group = init_group };
		      case er of
			  BARE =>
			      (bare_preload BtNames.bare_preloads;
			       system_values := SrcPathMap.empty;
			       NONE)
			| AUTOLOAD =>
			      (HostMachDepVC.Interact.installCompManager
			            (SOME al_manager');
				    standard_preload BtNames.standard_preloads;
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
	    fun p (f, mk, ("sml" | "sig" | "fun")) =
		HostMachDepVC.Interact.useFile f
	      | p (f, mk, "cm") = mk f
	      | p (f, mk, e) = Say.say ["!* unable to process `", f,
					"' (unknown extension `", e, "')\n"]
	    fun badopt opt f () =
		Say.say ["!* bad ", opt, " option: `", f, "'\n"]
	    fun carg ("-D", f, _) =
		let val bad = badopt "-D" f
		in
		    case String.fields (fn c => c = #"=")
				       (String.extract (f, 2, NONE)) of
			"" :: _ => bad ()
		      | [var, num] =>
			(case Int.fromString num of
			     SOME i => #set (SSV.symval var) (SOME i)
			   | NONE => bad ())
		      | [var] => #set (SSV.symval var) (SOME 1)
		      | _ => bad ()
		end
	      | carg ("-U", f, _) =
		(case String.extract (f, 2, NONE) of
		     "" => badopt "-U" f ()
		   | var => #set (SSV.symval var) NONE)
	      | carg (_, f, mk) = p (f, mk,
				 String.map Char.toLower
					    (getOpt (OS.Path.ext f, "<none>")))

	    fun args ("-a" :: rest, _) = args (rest, autoload)
	      | args ("-m" :: rest, _) = args (rest, make)
	      | args ("@CMbuild" :: rest, _) = mlbuild rest
	      | args (f :: rest, mk) =
		(carg (String.substring (f, 0, 2), f, mk)
		 handle General.Subscript => ();
		 args (rest, mk))
	      | args ([], _) = ()
	in
	    case SMLofNJ.getArgs () of
		["@CMslave"] => (#set StdConfig.verbose false; slave ())
	      | l => args (l, autoload)
	end
    in
	initTheValues (bootdir, de, er,
		       fn () => (Cleanup.install initPaths;
				 procCmdLine))
    end

    structure CM = struct
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
	    type lib = SrcPath.file
	    val known = Parse.listLibs
	    val descr = SrcPath.descr
	    val osstring = SrcPath.osstring
	    val dismiss = Parse.dismissLib
	    fun unshare lib = (Link.unshare lib; dismiss lib)
	end

	structure State = struct
	    val synchronize = SrcPath.sync
	    val reset = reset
	    val pending = getPending
	end

	structure Server = struct
	    type server = Servers.server_handle
	    fun start x = Servers.start x
			  before SrcPath.scheduleNotification ()
	    val stop = Servers.stop
	    val kill = Servers.kill
	    val name = Servers.name
	end

	val autoload = autoload
	val make = make
	val recomp = recomp
	val stabilize = stabilize

	val sources = sources

	val symval = SSV.symval
	val load_plugin = cwd_load_plugin 
	val mk_standalone = mk_standalone
    end

    structure Tools = ToolsFn (val load_plugin' = load_plugin'
			       val penv = penv)

    val load_plugin = load_plugin
  end
end
