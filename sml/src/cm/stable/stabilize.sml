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

    type statenvgetter = GP.info -> DG.bnode -> E.staticEnv
    type recomp = GP.info -> GG.group -> bool
    type pid = Pid.persstamp
in

signature STABILIZE = sig

    val loadStable :
	GP.info * (SrcPath.t -> GG.group option) * bool ref ->
	SrcPath.t -> GG.group option

    val stabilize :
	GP.info -> { group: GG.group, anyerrors: bool ref } ->
	GG.group option
end

functor StabilizeFn (val bn2statenv : statenvgetter
		     val transfer_state : SmlInfo.info * BinInfo.info -> unit
		     val recomp : recomp) :> STABILIZE = struct

    structure SSMap = BinaryMapFn
	(struct
	     type ord_key = SymbolSet.set
	     val compare = SymbolSet.compare
	end)

    structure SNMap = BinaryMapFn
	(struct
	     type ord_key = DG.snode
	     fun compare (DG.SNODE n, DG.SNODE n') =
		 SmlInfo.compare (#smlinfo n, #smlinfo n')
	end)

    type 'a maps = { ss: 'a SSMap.map, sn: 'a SNMap.map }

    val initMap = { ss = SSMap.empty, sn = SNMap.empty }

    structure PU = PickleUtilFn (type 'a map = 'a maps val emptyMap = initMap)
    structure PSym = PickleSymbolFn (structure PU = PU)
    structure UU = UnpickleUtil

    infix 3 $
    infixr 4 &
    val op & = PU.&
    val % = PU.%

    (* type info *)
    val (BN, SN, SBN, SS, SI, FSBN, IMPEXP, SHM) = (1, 2, 3, 4, 5, 6, 7, 8)

    val SSs = { find = fn (m: 'a maps, k) => SSMap.find (#ss m, k),
	        insert = fn ({ ss, sn }, k, v) =>
		             { sn = sn, ss = SSMap.insert (ss, k, v) } }
    val SNs = { find = fn (m: 'a maps, k) => SNMap.find (#sn m, k),
	        insert = fn ({ ss, sn }, k, v) =>
		             { ss = ss, sn = SNMap.insert (sn, k, v) } }

    fun genStableInfoMap (exports, group) = let
	(* find all the exported bnodes that are in the same group: *)
	fun add (((_, DG.SB_BNODE (n as DG.BNODE b)), _), m) = let
	    val i = #bininfo b
	in
	    if SrcPath.compare (BinInfo.group i, group) = EQUAL then
		IntBinaryMap.insert (m, BinInfo.offset i, n)
	    else m
	end
	  | add (_, m) = m
    in
	SymbolMap.foldl add IntBinaryMap.empty exports
    end

    fun stabilize gp { group = g as GG.GROUP grec, anyerrors } = let

	val primconf = #primconf (#param gp)
	val policy = #fnpolicy (#param gp)

	val grouppath = #grouppath grec

	fun doit wrapped = let

	    val _ =
		if StringSet.isEmpty wrapped then ()
		else
		    Say.say ("$Stabilize: wrapping the following privileges:\n"
			     :: map (fn s => ("  " ^ s ^ "\n"))
			            (StringSet.listItems wrapped))

	    val bname = SmlInfo.binname
	    val bsz = OS.FileSys.fileSize o bname

	    fun cpb s i = let
		val N = 4096
		fun copy ins = let
		    fun cp () =
			if BinIO.endOfStream ins then ()
			else (BinIO.output (s, BinIO.inputN (ins, N));
			      cp ())
		in
		    cp ()
		end
	    in
		SafeIO.perform { openIt = fn () => BinIO.openIn (bname i),
				 closeIt = BinIO.closeIn,
				 work = copy,
				 cleanup = fn () => () }
	    end

	    val grpSrcInfo = (#errcons gp, anyerrors)

	    val exports = #exports grec
	    val required = StringSet.difference (#required grec, wrapped)
	    val sublibs = #sublibs grec

	    (* The format of a stable archive is the following:
	     *  - It starts with the size s of the pickled dependency
	     *    graph. This size itself is written as four-byte string.
	     *  - The pickled dependency graph.  This graph contains
	     *    integer offsets of the binfiles for the individual ML
	     *    members. These offsets need to be adjusted by adding
	     *    s + 4. The pickled dependency graph also contains integer
	     *    offsets relative to other stable groups.  These offsets
	     *    need no further adjustment.
	     *  - Individual binfile contents (concatenated).
	     *)

	    (* Here we build a mapping that maps each BNODE to a number
	     * representing the sub-library that it came from and a
	     * representative symbol that can be used to find the BNODE
	     * within the exports of that library *)
	    fun oneB i (sy, ((_, DG.SB_BNODE (DG.BNODE n)), _), m) =
		StableMap.insert (m, #bininfo n, (i, sy))
	      | oneB i (_, _, m) = m
	    fun oneSL ((g as GG.GROUP { exports, ... }), (m, i)) =
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

	    val int = PU.w_int
	    val symbol = PSym.w_symbol
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
		val offset = registerOffset (i, bsz i)
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

	    val op $ = PU.$ BN
	    fun bn (DG.PNODE p) = "1" $ primitive p
	      | bn (DG.BNODE { bininfo = i, ... }) = let
		    val (n, sy) = valOf (StableMap.find (inverseMap, i))
		in
		    "2" $ int n & symbol sy
		end

	    fun sn n = let
		fun raw_sn (DG.SNODE n) =
		    "a" $ si (#smlinfo n) & list sn (#localimports n) &
		    list fsbn (#globalimports n)
	    in
		share SNs raw_sn n
	    end

	    and sbn x = let
		val op $ = PU.$ SBN
	    in
		case x of
		    DG.SB_BNODE n => "a" $ bn n
		  | DG.SB_SNODE n => "b" $ sn n
	    end
	
	    and fsbn (f, n) = let
		val op $ = PU.$ FSBN
	    in
		"f" $ filter f & sbn n
	    end

	    fun impexp (s, (n, _)) = let
		val op $ = PU.$ IMPEXP
	    in
		"i" $ symbol s & fsbn n
	    end

	    fun w_exports e = list impexp (SymbolMap.listItemsi e)

	    fun privileges p = list string (StringSet.listItems p)

	    fun group () = let
		fun sg (GG.GROUP { grouppath, ... }) = abspath grouppath
	    in
		(* Pickle the sublibs first because we need to already
		 * have them back when we unpickle BNODEs. *)
		list sg sublibs & w_exports exports & privileges required
	    end

	    val pickle = PU.pickle (group ())
	    val sz = size pickle
	    val offset_adjustment = sz + 4

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
			    transfer_state (smlinfo, i);
			    m := SmlInfoMap.insert (!m, smlinfo, n);
			    n
			end

		and sbn (DG.SB_SNODE n) = sn n
		  | sbn (DG.SB_BNODE n) = n

		and fsbn (f, n) = (f, sbn n)

		fun impexp ((f, n), e) = ((f, DG.SB_BNODE (sbn n)), e)

		val exports = SymbolMap.map impexp (#exports grec)
		val simap = genStableInfoMap (exports, grouppath)
	    in
		GG.GROUP { exports = exports,
			   kind = GG.STABLELIB simap,
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

	    val gpath = #grouppath grec
	    fun mksname () = FilenamePolicy.mkStableName policy gpath
	    fun work outs =
		(Say.vsay ["[stabilizing ", SrcPath.descr gpath, "]\n"];
		 writeInt32 (outs, sz);
		 BinIO.output (outs, Byte.stringToBytes pickle);
		 app (cpb outs) memberlist;
		 mkStableGroup mksname)
	in
	    SOME (SafeIO.perform { openIt = AutoDir.openBinOut o mksname,
				   closeIt = BinIO.closeOut,
				   work = work,
				   cleanup = fn () =>
				    (OS.FileSys.remove (mksname ())
				     handle _ => ()) })
	    handle exn => NONE
	end
    in
	case #kind grec of
	    GG.STABLELIB _ => SOME g
	  | GG.NOLIB => EM.impossible "stabilize: no library"
	  | GG.LIB wrapped =>
		if not (recomp gp g) then
		    (anyerrors := true; NONE)
		else let
		    fun notStable (GG.GROUP { kind, ... }) =
			case kind of GG.STABLELIB _ => false | _ => true
		in
		    case List.filter notStable (#sublibs grec) of
			[] => doit wrapped
		      | l => let
			    val grammar = case l of [_] => " is" | _ => "s are"
			    fun ppb pps = let
				fun loop [] = ()
				  | loop (GG.GROUP { grouppath, ... } :: t) =
				    (PP.add_string pps
				        (SrcPath.descr grouppath);
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
		end
    end

    fun loadStable (gp, getGroup, anyerrors) group = let

	val es2bs = GenericVC.CoerceEnv.es2bs
	fun bn2env n =
	    Statenv2DAEnv.cvtMemo (fn () => es2bs (bn2statenv gp n))

	val errcons = #errcons gp
	val grpSrcInfo = (errcons, anyerrors)
	val gdescr = SrcPath.descr group
	fun error l = EM.errorNoFile (errcons, anyerrors) SM.nullRegion
	    EM.COMPLAIN (concat ("(stable) " :: gdescr :: ": " :: l))
	    EM.nullErrorBody

	exception Format = UU.Format

	val pcmode = #pcmode (#param gp)
	val policy = #fnpolicy (#param gp)
	val primconf = #primconf (#param gp)
	fun mksname () = FilenamePolicy.mkStableName policy group

	fun work s = let

	    fun getGroup' p =
		case getGroup p of
		    SOME g => g
		  | NONE => (error ["unable to find ", SrcPath.descr p];
			     raise Format)

	    (* for getting sharing right... *)
	    val m = ref IntBinaryMap.empty
	    val next = ref 0

	    val pset = ref PidSet.empty

	    fun bytesIn n = let
		val bv = BinIO.inputN (s, n)
	    in
		if n = Word8Vector.length bv then bv
		else raise Format
	    end

	    val sz = LargeWord.toIntX (Pack32Big.subVec (bytesIn 4, 0))
	    val pickle = Byte.bytesToString (bytesIn sz)
	    val offset_adjustment = sz + 4

	    val session = UU.mkSession (UU.stringGetter pickle)

	    fun list m r = UU.r_list session m r
	    fun option m r = UU.r_option session m r
	    val int = UU.r_int session
	    fun share m r = UU.share session m r
	    fun nonshare r = UU.nonshare session r
	    val string = UU.r_string session
	    val symbol = UnpickleSymbol.r_symbol (session, string)
	    val bool = UU.r_bool session

	    val stringListM = UU.mkMap ()
	    val symbolListM = UU.mkMap ()
	    val stringListM = UU.mkMap ()
	    val ssM = UU.mkMap ()
	    val ssoM = UU.mkMap ()
	    val boolOptionM = UU.mkMap ()
	    val siM = UU.mkMap ()
	    val sgListM = UU.mkMap ()
	    val snM = UU.mkMap ()
	    val snListM = UU.mkMap ()
	    val bnM = UU.mkMap ()
	    val sbnM = UU.mkMap ()
	    val fsbnM = UU.mkMap ()
	    val fsbnListM = UU.mkMap ()
	    val impexpM = UU.mkMap ()
	    val impexpListM = UU.mkMap ()

	    val stringlist = list stringListM string

	    fun abspath () =
		SrcPath.unpickle pcmode (stringlist (), group)
		handle SrcPath.Format => raise Format
		     | SrcPath.BadAnchor a =>
		       (error ["configuration anchor \"", a, "\" undefined"];
			raise Format)

	    val symbollist = list symbolListM symbol

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

	    fun sg () = getGroup' (abspath ())

	    val sublibs = list sgListM sg ()

	    fun bn () = let
		fun bn' #"1" = DG.PNODE (primitive ())
		  | bn' #"2" = let
			val n = int ()
			val sy = symbol ()
			val GG.GROUP { exports = slexp, ... } =
			    List.nth (sublibs, n) handle _ => raise Format
		    in
			case SymbolMap.find (slexp, sy) of
			    SOME ((_, DG.SB_BNODE (n as DG.BNODE _)), _) => n
			  | _ => raise Format
		    end
		  | bn' _ = raise Format
	    in
		share bnM bn'
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
		fun sbn' #"a" = bn ()
		  | sbn' #"b" = sn ()
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
			val e = bn2env n
			(* put a filter in front to avoid having the FCTENV
			 * being queried needlessly (this avoids spurious
			 * module loadings) *)
			val e' = DAEnv.FILTER (SymbolSet.singleton sy, e)
		    in
			(* coerce to farsbnodes *)
			(sy, ((f, DG.SB_BNODE n), e'))
		    end
		  | ie _ = raise Format
	    in
		share impexpM ie
	    end

	    val impexplist = list impexpListM impexp

	    fun r_exports () =
		foldl SymbolMap.insert' SymbolMap.empty (impexplist ())

	    val stringlist = list stringListM string

	    fun privileges () =
		StringSet.addList (StringSet.empty, stringlist ())

	    val exports = r_exports ()
	    val required = privileges ()
	    val simap = genStableInfoMap (exports, group)
	in
	    GG.GROUP { exports = exports,
		       kind = GG.STABLELIB simap,
		       required = required,
		       grouppath = group,
		       sublibs = sublibs }
	end
    in
	SOME (SafeIO.perform { openIt = BinIO.openIn o mksname,
			       closeIt = BinIO.closeIn,
			       work = work,
			       cleanup = fn () => () })
	handle Format => NONE
             | IO.Io _ => NONE
    end
end

end (* local *)
