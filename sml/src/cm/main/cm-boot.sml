(*
 * This is the module that actually puts together the contents of the
 * structure CM people find in $smlnj/cm/full.cm.
 *
 *   Copyright (c) 1999, 2000 by Lucent Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
functor LinkCM (structure HostBackend : BACKEND) = struct

  datatype envrequest = AUTOLOAD | BARE

  local
      structure E = Environment
      structure DE = DynamicEnv
      structure SE = StaticEnv
      structure ER = EnvRef
      structure S = Symbol
      structure EM = ErrorMsg
      structure P = OS.Path
      structure F = OS.FileSys
      structure DG = DependencyGraph
      structure GG = GroupGraph
      structure IM = IntMap

      val os = SMLofNJ.SysInfo.getOSKind ()
      val my_archos =
	  concat [HostBackend.architecture, "-", FilenamePolicy.kind2name os]

      structure SSV =
	  SpecificSymValFn (val arch = HostBackend.architecture
			    val os = os)

      val system_values =
	  ref (SrcPathMap.empty: E.dynenv IntMap.map SrcPathMap.map)

      structure StabModmap = StabModmapFn ()

      val useStreamHook =
	  ref (fn _ => raise Fail "useStreamHook not initialized")
	  : (TextIO.instream -> unit) ref

      structure Compile =
	  CompileFn (structure Backend = HostBackend
		     structure StabModmap = StabModmap
		     fun useStream s = !useStreamHook s
		     val compile_there = Servers.compile o SrcPath.encode)

      structure BFC =
          BfcFn (val arch = HostBackend.architecture)

      structure Link =
	  LinkFn (val x = 1		(* ***** *)
                  structure BFC = BFC
		  val system_values = system_values)

      structure AutoLoad = AutoLoadFn
	  (structure C = Compile
	   structure L = Link
	   structure BFC = BFC)

      val mkBootList = #l o MkBootList.group (fn p => p)

      fun init_servers (GG.GROUP { grouppath, ... }) =
	  Servers.cm { archos = my_archos, project = SrcPath.encode grouppath }
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
	      val { group = l_group, ... } =
		  Link.newTraversal (g, #contents o get)
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
				    val loc = ER.loc ()
				    val base = #get loc ()
				    val new = E.concatEnv (delta, base)
				in
				    #set loc new;
				    Say.vsay ["[New bindings added.]\n"]
				end
			    else ();
			    true))
	  end

      val al_greg = GroupReg.new ()

      (* Instantiate the stabilization mechanism. *)
      structure Stabilize =
	  StabilizeFn (val arch = HostBackend.architecture
		       structure StabModmap = StabModmap
		       fun recomp gp g = let
			   val { store, get } = BFC.new ()
			   val { group, ... } =
			       Compile.newTraversal (Link.evict, store, g)
		       in
			   case group gp of
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
	      { os = os, arch = HostBackend.architecture }

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

	  fun getPending () =
	      map (Symbol.describe o #1)
		  (SymbolMap.listItemsi (AutoLoad.getPending ()))

	  fun showBindings () = let
	      val loaded = map Symbol.describe (EnvRef.listBoundSymbols ())
	      val pending = getPending ()
	      fun pr s = Say.say [s, "\n"]
	  in
	      Say.say ["\n*** Symbols bound at toplevel:\n"];
	      app pr loaded;
	      Say.say ["\n*** Symbols registered for autoloading:\n"];
	      app pr pending
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

	  fun param slave_mode =
	      { fnpolicy = fnpolicy,
		penv = penv,
		symval = SSV.symval,
		archos = my_archos,
		keep_going = #get StdConfig.keep_going (),
		slave_mode = slave_mode }

	  val init_group = #init_group o getTheValues

	  fun dropPickles () =
	      if #get StdConfig.conserve_memory () then
		  Parse.dropPickles ()
	      else ()

	  fun parse_arg0 slave_mode (gr, sflag, p) =
	      { load_plugin = load_plugin, gr = gr, param = param slave_mode,
	        stabflag = sflag, group = p,
		init_group = init_group (), paranoid = false }

	  and parse_arg x = parse_arg0 false x

	  and slave_parse_arg x = parse_arg0 true x

	  and autoload s = let
	      val p = mkStdSrcPath s
	  in
	      (case Parse.parse (parse_arg (al_greg, NONE, p)) of
		   NONE => false
		 | SOME (g, _) =>
		   (AutoLoad.register (EnvRef.loc (), g);
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

	  fun stabilize recursively root = let
	      fun stabilize_recomp_runner gp g = let
		  val _ = init_servers g
		  val { allgroups, ... } =
		      Compile.newTraversal (Link.evict, fn _ => (), g)
	      in
		  Servers.withServers (fn () => allgroups gp)
	      end
	      fun stabilize_dummy_runner gp g = true
	      fun phase1 () = run mkStdSrcPath NONE
				  stabilize_recomp_runner root
	      fun phase2 () = (Compile.reset ();(* a bit too draconian? *)
			       run mkStdSrcPath (SOME recursively)
				   stabilize_dummy_runner root)
	  in
	      (* Don't bother with the 2-phase thing if there are
	       * no compile servers attached.  (We still need
	       * the "withServers" call to clean up our queues in case
	       * of an interrupt or error.) *)
	      if Servers.noServers () then Servers.withServers phase2
	      else
		  (* We do this in two phases:
		   *    1. recompile everything without stabilization but
		   *       potentially using compile servers
		   *    2. do a local stabilization run (which should have
		   *       no need to compile anything); don't use servers
		   *)
		  phase1 () andalso phase2 ()
	  end

	  val recomp = run mkStdSrcPath NONE recomp_runner
	  val make = run mkStdSrcPath NONE (make_runner true)

	  fun to_portable s = let
	      val gp = mkStdSrcPath s
	      fun nativesrc s = let
		  val p = SrcPath.standard
			      { err = fn s => raise Fail s, env = penv }
			      { context = SrcPath.dir gp, spec = s }
	      in
		  SrcPath.osstring' (SrcPath.file p)
	      end
	      fun mkres (g, pl) = { graph = g, imports = pl,
				    nativesrc = nativesrc }
	  in
	      Option.map
		  (mkres o ToPortable.export)
		  (Parse.parse (parse_arg
				    (GroupReg.new (), NONE, mkStdSrcPath s)))
	  end

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
	      fun parse p = Parse.parse (slave_parse_arg (gr, NONE, p))
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

	  fun al_ginfo () = { param = param false,
			      groupreg = al_greg,
			      errcons = EM.defaultConsumer (),
			      youngest = ref TStamp.ancient }

	  val al_manager =
	      AutoLoad.mkManager { get_ginfo = al_ginfo,
				   dropPickles = dropPickles }

	  fun reset () =
	      (Compile.reset ();
	       Link.reset ();
	       AutoLoad.reset ();
	       Parse.reset ();
	       SmlInfo.reset ();
	       StabModmap.reset ())

	  fun initTheValues (bootdir, de, er, autoload_postprocess, icm) = let
	      (* icm: "install compilation manager" *)
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
			      val fromHex = PersStamps.fromHex
			  in
			      case String.tokens (fn c => c = #":") spec of
				  [pos, hexp] =>
				  (case (fromHex hexp, Int.fromString pos) of
				       (SOME p, SOME i) =>
				       (case DE.look de p of
					    NONE => pm
					  | SOME obj => 
					    IM.insert (pm, i,
						       DE.singleton (p, obj)))
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
				      archos = my_archos,
				      keep_going = false,
				      slave_mode = false },
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
		      #set (ER.loc ()) E.emptyEnv;(* redundant? *)
		      theValues := SOME { init_group = init_group };
		      case er of
			  BARE =>
			      (bare_preload BtNames.bare_preloads;
			       system_values := SrcPathMap.empty;
			       NONE)
			| AUTOLOAD =>
			      (icm al_manager;
			       standard_preload BtNames.standard_preloads;
			       (* unconditionally drop all library pickles *)
			       Parse.dropPickles ();
			       SOME (autoload_postprocess ()))
		  end
	  end
      end
  in
    fun init (bootdir, de, er, useStream, useFile, icm) = let
	fun procCmdLine () = let
	    val autoload = ignore o autoload
	    val make = ignore o make
	    fun p (f, mk, ("sml" | "sig" | "fun")) = useFile f
	      | p (f, mk, "cm") = mk f
	      | p (f, mk, e) = Say.say ["!* unable to process `", f,
					"' (unknown extension `", e, "')\n"]

	    fun show_controls (first, getarg, getval, pad) level = let
		fun one (c : Controls.control, prev) = let
		    val rname = #rname c
		    val arg = getarg c
		    val value = getval c
		    val sz = size value
		    val lw = !Control_Print.linewidth
		    val padsz = lw - 6 - size arg
		in
		    if prev = rname then ()
		    else Say.say ["    ", rname, ":\n"];
		    if padsz < sz then
			let val padsz' = Int.max (lw, sz + 8)
			in
			    Say.say ["      ", arg, "\n",
				     StringCvt.padLeft #" " padsz' value,
				     "\n"]
			end
		    else Say.say ["      ", arg, pad padsz value, "\n"];
		    rname
		end
		val cl = Controls.controls level
	    in
		if List.null cl then ()
		else (first (); ignore (foldl one "" cl))
	    end

	    fun help level =
	       (Say.say
		    ["sml [rtsargs] [options] [files]\n\
		     \\n\
		     \  rtsargs:\n\
		     \    @SMLload=<h>     (start specified heap image)\n\
		     \    @SMLalloc=<s>    (specify size of allocation area)\n\
		     \    @SMLcmdname=<n>  (set command name)\n\
		     \    @SMLquiet        (load heap image silently)\n\
		     \    @SMLverbose      (show heap image load progress)\n\
		     \    @SMLobjects      (show list of executable objects)\n\
		     \    @SMLdebug=<f>    (write debugging info to file)\n\
		     \\n\
		     \  files:\n\
		     \    <file>.cm        (CM.make or CM.autoload)\n\
		     \    -m               (switch to CM.make)\n\
		     \    -a               (switch to CM.autoload; default)\n\
		     \    <file>.sig       (use)\n\
		     \    <file>.sml       (use)\n\
		     \    <file>.fun       (use)\n\
		     \\n\
		     \  options:\n\
		     \    -D<name>=<v>     (set CM variable to given value)\n\
		     \    -D<name>         (set CM variable to 1)\n\
		     \    -Uname           (unset CM variable)\n\
		     \    -C<control>=<v>  (set named control)\n\
		     \    -H               (produce complete help listing)\n\
		     \    -h               (produce minimal help listing)\n\
		     \    -h<level>        (help with obscurity limit)\n\
		     \    -S               (list all current settings)\n\
		     \    -s<level>        (limited list of settings)\n"];
		show_controls (fn () => Say.say ["\n  controls:\n"],
			       #name,
			       fn c => concat ["(", #descr c, ")"],
			       StringCvt.padLeft #" ")
			      level)

	    fun showcur level = let
		fun nopad (_, s) = s
	    in
		show_controls (fn () => (),
			       fn c => (#name c ^ "="),
			       fn c => #get (#svar c) (),
			       fn _ => fn s => s)
			      level
	    end

	    fun badopt opt f () =
		Say.say ["!* bad ", opt, " option: `", f, "'\n",
			 "!* try `-h' or `-h<level>' for help\n"]
	    fun carg (opt as ("-C" | "-D"), f, _) =
		let val bad = badopt opt f
		    val spec = Substring.extract (f, 2, NONE)
		    val is_config = opt = "-C"
		    val (name, value) =
			Substring.splitl (fn c => c <> #"=") spec
		    val name = Substring.string name
		    val value = Substring.string
				    (if Substring.size value > 0 then
					 Substring.slice (value, 1, NONE)
				     else value)
		in
		    if name = "" then bad ()
		    else if is_config then
			let val svar = #svar (Controls.control name)
			in
			    #set svar value
			    handle Controls.FormatError { t, s } =>
				   Say.say ["!* unable to parse value `", s,
					    "' for ", name, " : ", t, "\n"]
			end handle Controls.NoSuchControl =>
				   Say.say ["!* no such control: ",
					    name, "\n"]
		    else if value = "" then #set (SSV.symval name) (SOME 1)
		    else (case Int.fromString value of
			      SOME i => #set (SSV.symval name) (SOME i)
			    | NONE => bad ())
		end
	      | carg ("-U", f, _) =
		(case String.extract (f, 2, NONE) of
		     "" => badopt "-U" f ()
		   | var => #set (SSV.symval var) NONE)
	      | carg ("-h", f, _) =
		(case String.extract (f, 2, NONE) of
		     "" => help (SOME 0)
		   | level => help (Int.fromString level))
	      | carg ("-s", f, _) =
		(case String.extract (f, 2, NONE) of
		     "" => showcur (SOME 0)
		   | level => showcur (Int.fromString level))
	      | carg (_, f, mk) = p (f, mk,
				 String.map Char.toLower
					    (getOpt (OS.Path.ext f, "<none>")))

	    fun args ("-a" :: rest, _) = args (rest, autoload)
	      | args ("-m" :: rest, _) = args (rest, make)
	      | args ("-H" :: rest, mk) = (help NONE; args (rest, mk))
	      | args ("-S" :: rest, mk) = (showcur NONE; args (rest, mk))
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
	useStreamHook := useStream;
	initTheValues (bootdir, de, er,
		       fn () => (Cleanup.install initPaths;
				 procCmdLine),
		       icm)
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
	    val generate_index = StdConfig.generate_index
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
	    val showBindings = showBindings
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

	structure Graph = struct
	    val graph = to_portable
	end
    end

    structure Tools = ToolsFn (val load_plugin' = load_plugin'
			       val penv = penv)

    val load_plugin = load_plugin
  end
end
