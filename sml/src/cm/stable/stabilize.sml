(*
 * Reading, generating, and writing stable groups.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
local
    structure DG = DependencyGraph
    structure GG = GroupGraph
    structure EM = GenericVC.ErrorMsg
    structure PP = PrettyPrint
    structure SM = GenericVC.SourceMap
    structure GP = GeneralParams
    structure E = GenericVC.Environment
    structure Pid = GenericVC.PersStamps
    structure P = PickMod
    structure UP = UnpickMod
    structure E = GenericVC.Environment
in

signature STABILIZE = sig

    val loadStable :
	GP.info -> { getGroup: SrcPath.t -> GG.group option,
		     anyerrors: bool ref }
	-> SrcPath.t -> GG.group option

    val stabilize :
	GP.info -> { group: GG.group, anyerrors: bool ref } -> GG.group option
end

functor StabilizeFn (val destroy_state : GP.info -> SmlInfo.info -> unit
		     structure MachDepVC : MACHDEP_VC
		     val recomp : GP.info -> GG.group ->
			 (SmlInfo.info -> MachDepVC.Binfile.bfContent) option
		     val getII : SmlInfo.info -> IInfo.info) :> STABILIZE =
struct

    structure BF = MachDepVC.Binfile

    structure SSMap = MapFn
	(struct
	     type ord_key = SymbolSet.set
	     val compare = SymbolSet.compare
	end)

    structure SNMap = MapFn
	(struct
	     type ord_key = DG.snode
	     fun compare (DG.SNODE n, DG.SNODE n') =
		 SmlInfo.compare (#smlinfo n, #smlinfo n')
	end)

    structure PU = PickleUtil
    structure UU = UnpickleUtil

    type map = { ss: PU.id SSMap.map, sn: PU.id SNMap.map, pm: P.map }

    val emptyMap = { ss = SSMap.empty, sn = SNMap.empty, pm = P.emptyMap }

    val lifter =
	{ extract = fn (m: map) => #pm m,
	  patchback = fn (m: map, pm) => { ss = #ss m, sn = #sn m, pm = pm } }

    infix 3 $
    infixr 4 &
    val op & = PU.&
    val % = PU.%

    (* type info *)
    val (BN, SN, SBN, SS, SI, FSBN, IMPEXP, SHM) = (1, 2, 3, 4, 5, 6, 7, 8)

    val SSs =
	{ find = fn (m: map, k) => SSMap.find (#ss m, k),
	  insert = fn ({ ss, sn, pm }, k, v) =>
	               { sn = sn, ss = SSMap.insert (ss, k, v), pm = pm } }
    val	SNs =
	{ find = fn (m: map, k) => SNMap.find (#sn m, k),
	  insert = fn ({ ss, sn, pm }, k, v) =>
	               { ss = ss, sn = SNMap.insert (sn, k, v), pm = pm } }

    fun fetch_pickle s = let
	fun bytesIn n = let
	    val bv = BinIO.inputN (s, n)
	in
	    if n = Word8Vector.length bv then bv
	    else raise UU.Format
	end

	val dg_sz = LargeWord.toIntX (Pack32Big.subVec (bytesIn 4, 0))
	val dg_pickle = Byte.bytesToString (bytesIn dg_sz)
    in
	{ size = dg_sz, pickle = dg_pickle }
    end

    fun mkPickleFetcher mksname () =
	SafeIO.perform { openIt = BinIO.openIn o mksname,
			 closeIt = BinIO.closeIn,
			 work = #pickle o fetch_pickle,
			 cleanup = fn _ => () }

    fun stabilize gp { group = g as GG.GROUP grec, anyerrors } = let

	val primconf = #primconf (#param gp)
	val policy = #fnpolicy (#param gp)
	val pervasive = #pervasive (#param gp)

	val grouppath = #grouppath grec

	fun doit (wrapped, getBFC) = let

	    fun writeBFC s i = BF.write { stream = s,
					  content = getBFC i,
					  nopickle = true }
	    fun sizeBFC i = BF.size { content = getBFC i, nopickle = true }

	    val _ =
		Say.vsay ["[stabilizing ", SrcPath.descr grouppath, "]\n"]

	    val _ =
		if StringSet.isEmpty wrapped then ()
		else
		    Say.say ("$Stabilize: wrapping the following privileges:\n"
			     :: map (fn s => ("  " ^ s ^ "\n"))
			            (StringSet.listItems wrapped))

	    val grpSrcInfo = (#errcons gp, anyerrors)

	    val exports = #exports grec
	    val required = StringSet.difference (#required grec, wrapped)
	    val sublibs = #sublibs grec

	    (* The format of a stable archive is the following:
	     *  - It starts with the size s of the pickled dependency
	     *    graph. This size itself is written as four-byte string.
	     *  - The size t of the pickled environment for the entire
	     *    library (using the pickleEnvN interface of the pickler)
	     *    in the same format as s.
	     *  - The pickled dependency graph.  This graph contains
	     *    integer offsets of the binfiles for the individual ML
	     *    members. These offsets need to be adjusted by adding
	     *    s + t + 8. The pickled dependency graph also contains integer
	     *    offsets relative to other stable groups.  These offsets
	     *    need no further adjustment.
	     *  - Individual binfile contents (concatenated) but without
	     *    their static environments.
	     *)

	    (* Here we build a mapping that maps each BNODE to a number
	     * representing the sub-library that it came from and a
	     * representative symbol that can be used to find the BNODE
	     * within the exports of that library *)
	    fun oneB i (sy, ((_, DG.SB_BNODE (DG.BNODE n, _)), _), m) =
		StableMap.insert (m, #bininfo n, (i, sy))
	      | oneB i (_, _, m) = m
	    fun oneSL ((_, g as GG.GROUP { exports, ... }), (m, i)) =
		(SymbolMap.foldli (oneB i) m exports, i + 1)
	    val inverseMap = #1 (foldl oneSL (StableMap.empty, 0) sublibs)

	    val members = ref []
	    val (registerOffset, getOffset) = let
		val dict = ref SmlInfoMap.empty
		val cur = ref 0
		fun reg (i, sz) = let
		    val os = !cur
		in
		    cur := os + sz;
		    dict := SmlInfoMap.insert (!dict, i, os);
		    members := i :: (!members);
		    os
		end
		fun get i = valOf (SmlInfoMap.find (!dict, i))
	    in
		(reg, get)
	    end

	    (* Collect all BNODEs and PNODEs that we see and build
	     * a context suitable for P.envPickler. *)
	    fun mkContext () = let
		fun lst f [] k s = k s
		  | lst f (h :: t) k s = f h (lst f t k) s

		fun sbn n k (s as (prims, bnodes, snodes)) =
		    case n of
			DG.SB_BNODE (DG.PNODE p, { statenv, ... }) => let
			    val str = String.str (Primitive.toIdent primconf p)
			    val prims' =
				StringMap.insert (prims, str, #env o statenv)
			in
			    k (prims', bnodes, snodes)
			end
		      | DG.SB_BNODE (DG.BNODE { bininfo = i, ... }, ii) => let
			    val { statenv, ... } = ii
			    val nsy = valOf (StableMap.find (inverseMap, i))
			    val bnodes' =
				StableMap.insert (bnodes, i,
						  (nsy, #env o statenv))
			in
			    k (prims, bnodes', snodes)
			end
		      | DG.SB_SNODE n => sn n k s

		and sn (DG.SNODE n) k (prims, bnodes, snodes) = let
		    val i = #smlinfo n
		    val li = #localimports n
		    val gi = #globalimports n
		in
		    if SmlInfoSet.member (snodes, i) then
			k (prims, bnodes, snodes)
		    else let
			val snodes' = SmlInfoSet.add (snodes, i)
		    in
			lst sn li (lst fsbn gi k) (prims, bnodes, snodes')
		    end
		end

		and fsbn (_, n) k s = sbn n k s

		fun impexp (n, _) k s = fsbn n k s

		val (prims, bnodes) =
		    lst impexp (SymbolMap.listItems exports)
		        (fn (prims, bnodes, _) => (prims, bnodes))
			(StringMap.empty, StableMap.empty, SmlInfoSet.empty)

		val priml = StringMap.listItemsi prims
		val bnodel = StableMap.listItems bnodes

		fun cvt lk id = let
		    fun nloop [] = NONE
		      | nloop ((k, ge) :: t) =
			(case lk (ge ()) id of
			     SOME _ => SOME (P.NodeKey k)
			   | NONE => nloop t)
		    fun ploop [] = nloop bnodel
		      | ploop ((k, ge) :: t) =
			(case lk (ge ()) id of
			     SOME _ => SOME (P.PrimKey k)
			   | NONE => ploop t)
		in
		    case lk (E.staticPart pervasive) id of
			NONE => ploop priml
		      | SOME _ => SOME (P.PrimKey "pv")
		end
	    in
		{ lookSTR = cvt GenericVC.CMStaticEnv.lookSTR,
		  lookSIG = cvt GenericVC.CMStaticEnv.lookSIG,
		  lookFCT = cvt GenericVC.CMStaticEnv.lookFCT,
		  lookFSIG = cvt GenericVC.CMStaticEnv.lookFSIG,
		  lookTYC = cvt GenericVC.CMStaticEnv.lookTYC,
		  lookEENV = cvt GenericVC.CMStaticEnv.lookEENV }
	    end

	    (* make the picklers for static and symbolic environments;
	     * lift them so we can use them here... *)
	    val envContext = mkContext ()

	    val env_orig = P.envPickler envContext
	    val env = PU.lift_pickler lifter env_orig
	    val symenv_orig = P.symenvPickler
	    val symenv = PU.lift_pickler lifter symenv_orig
	    val lazy_env = PU.w_lazy env
	    val lazy_symenv = PU.w_lazy symenv

	    val int = PU.w_int
	    val symbol = PickleSymPid.w_symbol
	    val pid = PickleSymPid.w_pid
	    val share = PU.ah_share
	    val option = PU.w_option
	    val list = PU.w_list
	    val string = PU.w_string
	    val bool = PU.w_bool
	    val int = PU.w_int

	    fun symbolset ss = let
		val op $ = PU.$ SS
		fun raw_ss ss = "s" $ list symbol (SymbolSet.listItems ss)
	    in
		share SSs raw_ss ss
	    end

	    val filter = option symbolset

	    fun shm (Sharing.SHARE true) = %SHM "a"
	      | shm (Sharing.SHARE false) = %SHM "b"
	      | shm Sharing.DONTSHARE = %SHM "c"

	    fun si i = let
		(* FIXME: this is not a technical flaw, but perhaps one
		 * that deserves fixing anyway:  If we only look at spec,
		 * then we are losing information about sub-grouping
		 * within libraries.  However, the spec in BinInfo.info
		 * is only used for diagnostics and has no impact on the
		 * operation of CM itself. *)
		val spec = SrcPath.specOf (SmlInfo.sourcepath i)
		val locs = SmlInfo.errorLocation gp i
		val offset = registerOffset (i, sizeBFC i)
		val sh_mode = SmlInfo.sh_mode i
		val op $ = PU.$ SI
	    in
		"s" $ string spec & string locs & int offset & shm sh_mode
	    end

	    fun primitive p =
		string (String.str (Primitive.toIdent primconf p))

	    fun warn_relabs p abs = let
		val relabs = if abs then "absolute" else "relative"
		fun ppb pps =
		    (PP.add_newline pps;
		     PP.add_string pps (SrcPath.descr p);
		     PP.add_newline pps;
		     PP.add_string pps
    "(This means that in order to be able to use the result of stabilization";
		     PP.add_newline pps;
		     PP.add_string pps "the library must be in the same ";
		     PP.add_string pps relabs;
		     PP.add_string pps " location as it is now.)";
		     PP.add_newline pps)
	    in
		EM.errorNoFile (#errcons gp, anyerrors) SM.nullRegion
		    EM.WARN
		    (concat [SrcPath.descr grouppath,
			     ": library referred to by ", relabs,
			     " pathname:"])
		    ppb
	    end

	    fun abspath p = let
		val pp = SrcPath.pickle (warn_relabs p) (p, grouppath)
	    in
		list string pp
	    end

	    fun sn n = let
		val op $ = PU.$ SN
		fun raw_sn (DG.SNODE n) =
		    "a" $ si (#smlinfo n) & list sn (#localimports n) &
		    list fsbn (#globalimports n)
	    in
		share SNs raw_sn n
	    end

	    (* Here we ignore the interface info because we will not
	     * need it anymore when we unpickle. *)
	    and sbn x = let
		val op $ = PU.$ SBN
	    in
		case x of
		    DG.SB_BNODE (DG.PNODE p, { statenv = getE, ... }) =>
			"1" $ primitive p
		  | DG.SB_BNODE (DG.BNODE { bininfo = i, ... }, _) => let
			val (n, sy) = valOf (StableMap.find (inverseMap, i))
		    in
			"2" $ int n & symbol sy
		    end
		  | DG.SB_SNODE n => "3" $ sn n
	    end
	
	    and fsbn (f, n) = let
		val op $ = PU.$ FSBN
	    in
		"f" $ filter f & sbn n
	    end

	    (* Here is the place where we need to write interface info. *)
	    fun impexp (s, (n, _)) = let
		val op $ = PU.$ IMPEXP
		val { statenv, symenv, statpid, sympid } =
		    case n of
			(_, DG.SB_BNODE (_, ii)) => ii
		      | (_, DG.SB_SNODE (DG.SNODE { smlinfo, ... })) =>
			    getII smlinfo
		fun es2bs { env, ctxt } =
		    { env = GenericVC.CoerceEnv.es2bs env, ctxt = ctxt }
	    in
		"i" $ symbol s & fsbn n &
		      lazy_env (es2bs o statenv) &
		      lazy_symenv symenv &
		      pid statpid &
		      pid sympid
	    end

	    fun w_exports e = list impexp (SymbolMap.listItemsi e)

	    fun privileges p = list string (StringSet.listItems p)

	    fun group () = let
		fun sg (p, g) = abspath p
	    in
		(* Pickle the sublibs first because we need to already
		 * have them back when we unpickle BNODEs. *)
		list sg sublibs & w_exports exports & privileges required
	    end

	    val dg_pickle =
		Byte.stringToBytes (PU.pickle emptyMap (group ()))

	    val dg_sz = Word8Vector.length dg_pickle

	    val offset_adjustment = dg_sz + 4

	    fun mkStableGroup mksname = let
		val m = ref SmlInfoMap.empty
		fun sn (DG.SNODE (n as { smlinfo, ... })) =
		    case SmlInfoMap.find (!m, smlinfo) of
			SOME n => n
		      | NONE => let
			    val li = map sn (#localimports n)
			    val gi = map fsbn (#globalimports n)
			    val sourcepath = SmlInfo.sourcepath smlinfo
			    (* FIXME: see the comment near the other
			     * occurence of SrcPath.spec... *)
			    val spec = SrcPath.specOf sourcepath
			    val offset =
				getOffset smlinfo + offset_adjustment
			    val sh_mode = SmlInfo.sh_mode smlinfo
			    val locs = SmlInfo.errorLocation gp smlinfo
			    val error = EM.errorNoSource grpSrcInfo locs
			    val i = BinInfo.new { group = grouppath,
						  mkStablename = mksname,
						  spec = spec,
						  offset = offset,
						  sh_mode = sh_mode,
						  error = error }
			    val n = DG.BNODE { bininfo = i,
					       localimports = li,
					       globalimports = gi }
			in
			    m := SmlInfoMap.insert (!m, smlinfo, n);
			    n
			end

		and sbn (DG.SB_SNODE (n as DG.SNODE { smlinfo = i, ... })) =
		    let val ii = getII i
		    in
			(sn n, ii)
		    end
		  | sbn (DG.SB_BNODE (n, ii)) = (n, ii)

		and fsbn (f, n) = (f, #1 (sbn n))

		fun impexp ((f, n), e) = ((f, DG.SB_BNODE (sbn n)), e)

		val exports = SymbolMap.map impexp (#exports grec)
	    in
		SmlInfoMap.appi (fn (i, _) => destroy_state gp i) (!m);
		GG.GROUP { exports = exports,
			   kind = GG.STABLELIB (fn () => ()),
			   required = required,
			   grouppath = grouppath,
			   sublibs = sublibs }
	    end

	    fun writeInt32 (s, i) = let
		val a = Word8Array.array (4, 0w0)
		val _ = Pack32Big.update (a, 0, LargeWord.fromInt i)
	    in
		BinIO.output (s, Word8Array.extract (a, 0, NONE))
	    end
	    val memberlist = rev (!members)

	    fun mksname () = FilenamePolicy.mkStableName policy grouppath
	    fun work outs =
		(writeInt32 (outs, dg_sz);
		 BinIO.output (outs, dg_pickle);
		 app (writeBFC outs) memberlist;
		 mkStableGroup mksname)
	in
	    SOME (SafeIO.perform { openIt = AutoDir.openBinOut o mksname,
				   closeIt = BinIO.closeOut,
				   work = work,
				   cleanup = fn _ =>
				    (OS.FileSys.remove (mksname ())
				     handle _ => ()) })
	    handle exn =>
		(EM.errorNoFile (#errcons gp, anyerrors) SM.nullRegion
		    EM.COMPLAIN
		    (concat ["Exception raised while stabilizing ",
			     SrcPath.descr grouppath])
		    EM.nullErrorBody;
		 NONE)
	end
    in
	case #kind grec of
	    GG.STABLELIB _ => SOME g
	  | GG.NOLIB => EM.impossible "stabilize: no library"
	  | GG.LIB wrapped =>
	     (case recomp gp g of
		  NONE => (anyerrors := true; NONE)
		| SOME bfc_acc => let
		      fun notStable (_, GG.GROUP { kind, ... }) =
			  case kind of GG.STABLELIB _ => false | _ => true
		  in
		    case List.filter notStable (#sublibs grec) of
			[] => doit (wrapped, bfc_acc)
		      | l => let
			    val grammar = case l of [_] => " is" | _ => "s are"
			    fun ppb pps = let
				fun loop [] = ()
				  | loop ((p, _) :: t) =
				    (PP.add_string pps (SrcPath.descr p);
				     PP.add_newline pps;
				     loop t)
			    in
				PP.add_newline pps;
				PP.add_string pps
				    (concat ["because the following sub-group",
					     grammar, " not stable:"]);
				PP.add_newline pps;
				loop l
			    end
			    val errcons = #errcons gp
			    val gdescr = SrcPath.descr (#grouppath grec)
			in
			    EM.errorNoFile (errcons, anyerrors) SM.nullRegion
			       EM.COMPLAIN
			       (gdescr ^ " cannot be stabilized")
			       ppb;
			    NONE
			end
		  end)
    end

    fun loadStable gp { getGroup, anyerrors } group = let

	val errcons = #errcons (gp: GeneralParams.info)
	val grpSrcInfo = (errcons, anyerrors)
	val gdescr = SrcPath.descr group
	fun error l = EM.errorNoFile (errcons, anyerrors) SM.nullRegion
	    EM.COMPLAIN (concat ("(stable) " :: gdescr :: ": " :: l))
	    EM.nullErrorBody

	exception Format = UU.Format

	val pcmode = #pcmode (#param gp)
	val policy = #fnpolicy (#param gp)
	val primconf = #primconf (#param gp)
	val pervasive = #pervasive (#param gp)

	fun mksname () = FilenamePolicy.mkStableName policy group

	fun work s = let

	    fun getGroup' p =
		case getGroup p of
		    SOME g => g
		  | NONE => (error ["unable to find ", SrcPath.descr p];
			     raise Format)

	    val { size = dg_sz, pickle = dg_pickle } = fetch_pickle s
	    val offset_adjustment = dg_sz + 4
	    val { getter, dropper } =
		UU.stringGetter' (SOME dg_pickle, mkPickleFetcher mksname)
	    val session = UU.mkSession getter

	    fun list m r = UU.r_list session m r
	    val string = UU.r_string session
	    val stringListM = UU.mkMap ()
	    val stringlist = list stringListM string

	    fun abspath () =
		SrcPath.unpickle pcmode (stringlist (), group)
		handle SrcPath.Format => raise Format
		     | SrcPath.BadAnchor a =>
		       (error ["configuration anchor \"", a, "\" undefined"];
			raise Format)

	    fun sg () = let
		val p = abspath ()
	    in
		(p, getGroup' p)
	    end
	    val sgListM = UU.mkMap ()
	    val sublibs = list sgListM sg ()

	    (* Now that we have the list of sublibs, we can build the
	     * environment for unpickling the environment list.
	     * We will need the environment list when unpickling the
	     * export list (making SB_BNODES). *)
	    fun prim_context "pv" = SOME (E.staticPart pervasive)
	      | prim_context s =
		SOME (E.staticPart (Primitive.env primconf
				    (valOf (Primitive.fromIdent primconf
					    (String.sub (s, 0))))))
		handle _ => NONE
	    fun node_context (n, sy) = let
		val (_, GG.GROUP { exports = slexp, ... }) =
		    List.nth (sublibs, n)
	    in
		case SymbolMap.find (slexp, sy) of
		    SOME ((_, DG.SB_BNODE (_, { statenv = ge, ... })), _) =>
			SOME (#env (ge ()))
		  | _ => NONE
	    end handle _ => NONE

	    val { symenv, env, symbol, symbollist } =
		UP.mkUnpicklers session
		    { prim_context = prim_context,
		      node_context = node_context }

	    val lazy_symenv = UU.r_lazy session symenv
	    val lazy_env = UU.r_lazy session env

	    fun option m r = UU.r_option session m r
	    val int = UU.r_int session
	    fun share m r = UU.share session m r
	    fun nonshare r = UU.nonshare session r
	    val bool = UU.r_bool session
	    val pid = UnpickleSymPid.r_pid string

	    val stringListM = UU.mkMap ()
	    val ssM = UU.mkMap ()
	    val ssoM = UU.mkMap ()
	    val boolOptionM = UU.mkMap ()
	    val siM = UU.mkMap ()
	    val snM = UU.mkMap ()
	    val snListM = UU.mkMap ()
	    val sbnM = UU.mkMap ()
	    val fsbnM = UU.mkMap ()
	    val fsbnListM = UU.mkMap ()
	    val impexpM = UU.mkMap ()
	    val impexpListM = UU.mkMap ()

	    fun symbolset () = let
		fun s #"s" = SymbolSet.addList (SymbolSet.empty, symbollist ())
		  | s _ = raise Format
	    in
		share ssM s
	    end

	    val filter = option ssoM symbolset

	    fun primitive () =
		valOf (Primitive.fromIdent primconf
		          (String.sub (string (), 0)))
		handle _ => raise Format

	    fun shm () = let
		fun s #"a" = Sharing.SHARE true
		  | s #"b" = Sharing.SHARE false
		  | s #"c" = Sharing.DONTSHARE
		  | s _ = raise Format
	    in
		nonshare s
	    end

	    fun si () = let
		fun s #"s" =
		    let val spec = string ()
			val locs = string ()
			val offset = int () + offset_adjustment
			val sh_mode = shm ()
			val error = EM.errorNoSource grpSrcInfo locs
		    in
			BinInfo.new { group = group,
				      mkStablename = mksname,
				      error = error,
				      spec = spec,
				      offset = offset,
				      sh_mode = sh_mode }
		    end
		  | s _ = raise Format
	    in
		share siM s
	    end

	    (* this is the place where what used to be an
	     * SNODE changes to a BNODE! *)
	    fun sn () = let
		fun sn' #"a" =
		    DG.BNODE { bininfo = si (),
			       localimports = snlist (),
			       globalimports = fsbnlist () }
		  | sn' _ = raise Format
	    in
		share snM sn'
	    end

	    and snlist () = list snListM sn ()

	    (* this one changes from farsbnode to plain farbnode *)
	    and sbn () = let
		fun sbn' #"1" = DG.PNODE (primitive ())
		  | sbn' #"2" = let
			val n = int ()
			val sy = symbol ()
			val (_, GG.GROUP { exports = slexp, ... }) =
			    List.nth (sublibs, n) handle _ => raise Format
		    in
			case SymbolMap.find (slexp, sy) of
			    SOME ((_, DG.SB_BNODE (n as DG.BNODE _, _)), _) =>
				n
			  | _ => raise Format
		    end
		  | sbn' #"3" = sn ()
		  | sbn' _ = raise Format
	    in
		share sbnM sbn'
	    end

	    and fsbn () = let
		fun f #"f" = (filter (), sbn ())
		  | f _ = raise Format
	    in
		share fsbnM f
	    end

	    and fsbnlist () = list fsbnListM fsbn ()

	    fun impexp () = let
		fun ie #"i" =
		    let val sy = symbol ()
			val (f, n) = fsbn () (* really reads farbnodes! *)
			val ge = lazy_env ()
			fun bs2es { env, ctxt } =
			    { env = GenericVC.CoerceEnv.bs2es env,
			     ctxt = ctxt }
			val ge' = bs2es o ge
			val ii = { statenv = Memoize.memoize ge',
				   symenv = lazy_symenv (),
				   statpid = pid (),
				   sympid = pid () }
			val e = Statenv2DAEnv.cvtMemo (#env o ge)
			(* put a filter in front to avoid having the FCTENV
			 * being queried needlessly (this avoids spurious
			 * module loadings) *)
			val e' = DAEnv.FILTER (SymbolSet.singleton sy, e)
		    in
			(sy, ((f, DG.SB_BNODE (n, ii)), e'))
		    end
		  | ie _ = raise Format
	    in
		share impexpM ie
	    end

	    val impexplist = list impexpListM impexp

	    fun r_exports () = let
		val iel = impexplist ()
	    in
		foldl SymbolMap.insert' SymbolMap.empty iel
	    end

	    val stringlist = list stringListM string

	    fun privileges () =
		StringSet.addList (StringSet.empty, stringlist ())

	    val exports = r_exports ()
	    val required = privileges ()
	in
	    GG.GROUP { exports = exports,
		       kind = GG.STABLELIB dropper,
		       required = required,
		       grouppath = group,
		       sublibs = sublibs }
	end
    in
	SOME (SafeIO.perform { openIt = BinIO.openIn o mksname,
			       closeIt = BinIO.closeIn,
			       work = work,
			       cleanup = fn _ => () })
	handle Format => (error ["file is corrupted (old version?)"];
			  NONE)
             | IO.Io _ => NONE
    end
end

end (* local *)
