(*
 * The bootstrap compiler.
 *   (Formerly known as "batch" compiler.)
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
local
    structure EM = GenericVC.ErrorMsg
    structure E = GenericVC.Environment
    structure SE = GenericVC.StaticEnv
    structure PS = GenericVC.PersStamps
    structure GG = GroupGraph
    structure DG = DependencyGraph
in
functor BootstrapCompileFn
	    (structure MachDepVC : MACHDEP_VC
	     val useStream : TextIO.instream -> unit
	     val os : SMLofNJ.SysInfo.os_kind
	     val load_plugin : SrcPath.dir -> string -> bool) =
struct
    structure SSV = SpecificSymValFn (structure MachDepVC = MachDepVC
				      val os = os)
    structure P = OS.Path
    structure F = OS.FileSys
    structure BF = MachDepVC.Binfile

    val arch = MachDepVC.architecture
    val osname = FilenamePolicy.kind2name os

    val archos = concat [arch, "-", osname]

    fun init_servers (GG.GROUP { grouppath, ... }) =
	Servers.cmb { archos = archos,
		      root = SrcPath.encode grouppath }
      | init_servers GG.ERRORGROUP = ()

    structure StabModmap = StabModmapFn ()

    structure Compile = CompileFn (structure MachDepVC = MachDepVC
				   structure StabModmap = StabModmap
				   val useStream = useStream
				   val compile_there =
				       Servers.compile o SrcPath.encode)

    structure BFC = BfcFn (structure MachDepVC = MachDepVC)

    (* instantiate Stabilize... *)
    structure Stabilize =
	StabilizeFn (structure MachDepVC = MachDepVC
		     structure StabModmap = StabModmap
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

    structure VerifyStable = VerStabFn (structure Stabilize = Stabilize)

    (* ... and Parse *)
    structure Parse = ParseFn (structure Stabilize = Stabilize
			       structure StabModmap = StabModmap
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

    local
	fun internal_reset () =
	    (Compile.reset ();
	     Parse.reset ();
	     StabModmap.reset ())
    in
        fun reset () =
	    (Say.vsay ["[CMB reset]\n"];
	     internal_reset ())
	val checkDirbase = let
	    val prev = ref NONE
	    fun ck db =
		(case !prev of
		     NONE => prev := SOME db
		   | SOME db' =>
		     if db = db' then ()
		     else (Say.vsay ["[new dirbase is `", db,
				     "'; CMB reset]\n"];
			   internal_reset ();
			   prev := SOME db))
	in
	    ck
	end
    end

    fun mk_compile { deliver, root, dirbase = dbopt, paranoid } = let

	val dirbase = getOpt (dbopt, BtNames.dirbaseDefault)
	val _ = checkDirbase dirbase
	val penvspec = BtNames.penvspec
	val initgspec = BtNames.initgspec
	val maingspec = BtNames.maingspec

	val bindir = concat [dirbase, BtNames.bin_infix, archos]
	val bootdir = concat [dirbase, BtNames.boot_infix, archos]

	val keep_going = #get StdConfig.keep_going ()

	val ctxt = SrcPath.cwd ()

	val listfile = P.joinDirFile { dir = bootdir, file = BtNames.bootlist }
	val pidmapfile = P.joinDirFile { dir = bootdir, file = BtNames.pidmap }

	val penv = SrcPath.newEnv ()
	val _ = SafeIO.perform { openIt = fn () => TextIO.openIn penvspec,
				 closeIt = TextIO.closeIn,
				 work = SrcPath.processSpecFile
					    { env = penv, specfile = penvspec,
					      say = Say.say },
				 cleanup = fn _ => () }
	val _ = SrcPath.sync ()

	fun stdpath s =
	    SrcPath.file (SrcPath.standard
			      { err = fn s => raise Fail s, env = penv }
			      { context = ctxt, spec = s })

	val initgspec = stdpath initgspec
	val maingspec =
	    case root of
		NONE => stdpath maingspec
	      | SOME r => SrcPath.decode penv r

	val fnpolicy =
	    FilenamePolicy.separate { bindir = bindir, bootdir = bootdir }
	        { arch = arch, os = os }

	val param =
	    { fnpolicy = fnpolicy,
	      penv = penv,
	      symval = SSV.symval,
	      keep_going = keep_going }

	val emptydyn = E.dynamicPart E.emptyEnv

	(* first, build an initial GeneralParam.info, so we can
	 * deal with the pervasive env and friends... *)

	val groupreg = GroupReg.new ()
	val errcons = EM.defaultConsumer ()
	val ginfo = { param = param, groupreg = groupreg,
		      errcons = errcons,
		      youngest = ref TStamp.ancient }

	fun mk_main_compile arg = let

	    val { pervasive = perv_n, others, src } = arg

	    fun recompInitGroup () = let
		val ovldR = GenericVC.Control.overloadKW
		val savedOvld = !ovldR
		val _ = ovldR := true
		val sbnode = Compile.newSbnodeTraversal ()

		val perv_fsbnode = (NONE, perv_n)

		fun rt n = valOf (sbnode ginfo n)
		val pervasive = rt perv_n

		fun rt2ie (n, ii: IInfo.info) = let
		    val s = #statenv ii ()
		    val (dae, mkDomain) = Statenv2DAEnv.cvt s
		    val domain = mkDomain ()
		in
		    { ie = (fn () => (NONE, n), dae, domain), domain = domain }
		end
		
		fun add_exports (n, exports) = let
		    val { ie, domain } = rt2ie (n, rt n)
		    fun ins_ie (sy, m) = SymbolMap.insert (m, sy, ie)
		in
		    SymbolSet.foldl ins_ie exports domain
		end

		val special_exports = let
		    fun mkie (n, rtn) = #ie (rt2ie (n, rtn))
		in
		    SymbolMap.insert (SymbolMap.empty,
				      PervAccess.pervStrSym,
				      mkie (perv_n, pervasive))
		end
	    in
		GG.GROUP { exports = foldl add_exports special_exports others,
			   kind = GG.LIB {
			     kind = GG.DEVELOPED { wrapped = StringSet.empty,
						   subgroups = [] },
				version = NONE },
			   required = StringSet.singleton "primitive",
			   grouppath = initgspec,
			   (* hack: sources never used for this group *)
			   sources = SrcPathMap.empty,
			   sublibs = [] }
		before (ovldR := savedOvld)
	    end

	    (* just go and load the stable init group or signal failure *)
	    fun loadInitGroup () = let
		val lsarg =
		    { getGroup = fn _ => raise Fail "CMB: initial getGroup",
		      anyerrors = ref false }
	    in
		case Stabilize.loadStable lsarg (ginfo, initgspec, NONE, []) of
		    NONE => NONE
		  | SOME (g as GG.GROUP { exports, ... }) => SOME g
		  | SOME GG.ERRORGROUP => NONE
	    end
		    
	    (* Don't try to load the stable init group. Instead, recompile
	     * directly. *)
	    fun dontLoadInitGroup () = let
		val g0 = recompInitGroup ()
		val stabarg = { group = g0, anyerrors = ref false }
	    in
		if deliver then
		    case Stabilize.stabilize ginfo stabarg of
			SOME g => g
		      | NONE => raise Fail "CMB: cannot stabilize init group"
		else g0
	    end

	    (* Try loading the init group from the stable file if possible;
	     * recompile if loading fails *)
	    fun tryLoadInitGroup () =
		case loadInitGroup () of
		    SOME g => g
		  | NONE => dontLoadInitGroup ()
			
	    (* Ok, now, based on "paranoid" and stable verification,
	     * call the appropriate function(s) to get the init group. *)
	    val init_group =
		if paranoid then let
		    val export_nodes = perv_n :: others
		    val ver_arg = (initgspec, export_nodes, [],
				   SrcPathSet.empty, NONE)
		    val em = StableMap.empty
		in
		    if VerifyStable.verify' ginfo em ver_arg then
			tryLoadInitGroup ()
		    else dontLoadInitGroup ()
		end
		else tryLoadInitGroup ()


	    val stab = if deliver then SOME true else NONE

	    val gr = GroupReg.new ()
	    val _ = GroupReg.register gr (initgspec, src)

	    val parse_arg =
		{ load_plugin = load_plugin,
		  gr = gr,
		  param = param,
		  stabflag = stab,
		  group = maingspec,
		  init_group = init_group,
		  paranoid = paranoid }
	in
	    Servers.dirbase dirbase;
	    Servers.cmb_new { archos = archos };
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
				val numitems = length bootitems
				fun biggerlen (s, n) = Int.max (size s, n)
				val maxlen = foldl biggerlen 0 bootitems
			    in
				wr (concat ["%", Int.toString numitems,
					    " ", Int.toString maxlen]);
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
				    app (fn str => TextIO.output (s, str))
					[" ", Int.toString os,
					 ":", PS.toHex pid]
			    end
			    fun writePidLine s (p, set) =
				if StableSet.isEmpty set then ()
				else (TextIO.output (s, SrcPath.encode p);
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
		    SOME ((g, gp, penv), thunk)
		end
	end handle Option => (Compile.reset (); NONE)
	    	   (* to catch valOf failures in "rt" *)
    in
	case BuildInitDG.build ginfo initgspec of
	    SOME x => mk_main_compile x
	  | NONE => NONE
    end

    fun compile dbopt =
	(StabModmap.reset ();
	 case mk_compile { deliver = true, root = NONE,
			   dirbase = dbopt, paranoid = true } of
	     NONE => false
	   | SOME (_, thunk) => thunk ())

    local
	fun slave NONE = (StabModmap.reset (); NONE)
	  | slave (SOME (dirbase, root)) =
	    case mk_compile { deliver = false, root = SOME root,
			      dirbase = SOME dirbase, paranoid = false } of
		NONE => NONE
	      | SOME ((g, gp, penv), _) => let
		    val trav = Compile.newSbnodeTraversal () gp
		    fun trav' sbn = isSome (trav sbn)
		in
		    SOME (g, trav', penv)
		end
    in
	val _ = CMBSlaveHook.init archos slave
    end

    val make' = compile
    fun make () = make' NONE
    val symval = SSV.symval
end
end (* local *)
