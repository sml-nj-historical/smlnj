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
		     val getPid : SmlInfo.info -> pid option
		     val warmup : BinInfo.info * pid -> unit
		     val recomp : recomp) :> STABILIZE = struct

    datatype pitem =
	PSS of SymbolSet.set
      | PS of Symbol.symbol
      | PSN of DG.snode

    datatype uitem =
	USS of SymbolSet.set
      | US of Symbol.symbol
      | UBN of DG.bnode

    fun compare (PS s, PS s') = SymbolOrdKey.compare (s, s')
      | compare (PS _, _) = GREATER
      | compare (_, PS _) = LESS
      | compare (PSS s, PSS s') = SymbolSet.compare (s, s')
      | compare (PSS _, _) = GREATER
      | compare (_, PSS _) = LESS
      | compare (PSN (DG.SNODE n), PSN (DG.SNODE n')) =
	SmlInfo.compare (#smlinfo n, #smlinfo n')

    structure Map =
	BinaryMapFn (struct
			 type ord_key = pitem
			 val compare = compare
	end)

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
		fun copy ins = let
		    fun cp () =
			if BinIO.endOfStream ins then ()
			else (BinIO.output (s, BinIO.input ins); cp ())
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

	    fun w_list w_item [] k m =
		"0" :: k m
	      | w_list w_item [a] k m =
		"1" :: w_item a k m
	      | w_list w_item [a, b] k m =
		"2" :: w_item a (w_item b k) m
	      | w_list w_item [a, b, c] k m =
		"3" :: w_item a (w_item b (w_item c k)) m
	      | w_list w_item [a, b, c, d] k m =
		"4" :: w_item a (w_item b (w_item c (w_item d k))) m
	      | w_list w_item (a :: b :: c :: d :: e :: r) k m =
		"5" :: w_item a (w_item b (w_item c (w_item d (w_item e
						     (w_list w_item r k))))) m

	    fun w_option w_item NONE k m = "n" :: k m
	      | w_option w_item (SOME i) k m = "s" :: w_item i k m

	    fun int_encode i = let
		(* this is the same mechanism that's also used in
		 * TopLevel/batch/binfile.sml (maybe we should share it) *)
		val n = Word32.fromInt i
		val // = LargeWord.div
		val %% = LargeWord.mod
		val !! = LargeWord.orb
		infix // %% !!
		val toW8 = Word8.fromLargeWord
		fun r (0w0, l) = Word8Vector.fromList l
		  | r (n, l) =
		    r (n // 0w128, toW8 ((n %% 0w128) !! 0w128) :: l)
	    in
		Byte.bytesToString (r (n // 0w128, [toW8 (n %% 0w128)]))
	    end

	    fun w_int i k m = int_encode i :: k m

	    fun w_share w C v k (i, m) =
		case Map.find (m, C v) of
		    SOME i' => "o" :: w_int i' k (i, m)
		  | NONE => "n" :: w v k (i + 1, Map.insert (m, C v, i))

	    fun w_symbol_raw s k m = let
		val ns = case Symbol.nameSpace s of
		    Symbol.SIGspace => "'"
		  | Symbol.FCTspace => "("
		  | Symbol.FSIGspace => ")"
		  | Symbol.STRspace => ""
		  | _ => GenericVC.ErrorMsg.impossible "stabilize:w_symbol"
	    in
		ns :: Symbol.name s :: "." :: k m
	    end

	    val w_symbol = w_share w_symbol_raw PS

	    val w_ss = w_share (w_list w_symbol o SymbolSet.listItems) PSS

	    val w_filter = w_option w_ss

	    fun w_string s k m = let
		fun esc #"\\" = "\\\\"
		  | esc #"\"" = "\\\""
		  | esc c = String.str c
	    in
		String.translate esc s :: "\"" :: k m
	    end

	    fun w_sharing NONE k m = "n" :: k m
	      | w_sharing (SOME true) k m = "t" :: k m
	      | w_sharing (SOME false) k m = "f" :: k m

	    fun w_si i k = let
		(* FIXME: this is not a technical flaw, but perhaps one
		 * that deserves fixing anyway:  If we only look at spec,
		 * then we are losing information about sub-grouping
		 * within libraries.  However, the spec in BinInfo.info
		 * is only used for diagnostics and has no impact on the
		 * operation of CM itself. *)
		val spec = SrcPath.specOf (SmlInfo.sourcepath i)
		val locs = SmlInfo.errorLocation gp i
		val offset = registerOffset (i, bsz i)
	    in
		w_string spec
		   (w_string locs
		         (w_int offset
			       (w_sharing (SmlInfo.share i) k)))
	    end

	    fun w_primitive p k m =
		String.str (Primitive.toIdent primconf p) :: k m

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

	    fun w_abspath p k m =
		w_list w_string (SrcPath.pickle (warn_relabs p) (p, grouppath))
		                k m

	    fun w_bn (DG.PNODE p) k m = "p" :: w_primitive p k m
	      | w_bn (DG.BNODE { bininfo = i, ... }) k m = let
		    val (n, sy) = valOf (StableMap.find (inverseMap, i))
		in
		    "b" :: w_int n (w_symbol sy k) m
		end

	    fun w_pid p = w_string (Byte.bytesToString (Pid.toBytes p))

	    fun w_sn_raw (DG.SNODE n) k =
		w_option w_pid (getPid (#smlinfo n))
		         (w_si (#smlinfo n)
			       (w_list w_sn (#localimports n)
				       (w_list w_fsbn (#globalimports n) k)))

	    and w_sn n = w_share w_sn_raw PSN n

	    and w_sbn (DG.SB_BNODE n) k m = "b" :: w_bn n k m
	      | w_sbn (DG.SB_SNODE n) k m = "s" :: w_sn n k m

	    and w_fsbn (f, n) k = w_filter f (w_sbn n k)

	    fun w_impexp (s, (n, _)) k = w_symbol s (w_fsbn n k)

	    fun w_exports e = w_list w_impexp (SymbolMap.listItemsi e)

	    fun w_bool true k m = "t" :: k m
	      | w_bool false k m = "f" :: k m

	    fun w_privileges p = w_list w_string (StringSet.listItems p)

	    fun pickle_group () = let
		fun w_sg (p, _) = w_abspath p
		fun k0 m = []
		val m0 = (0, Map.empty)
	    in
		(* Pickle the sublibs first because we need to already
		 * have them back when we unpickle BNODEs. *)
		concat (w_list w_sg sublibs
			    (w_exports exports
			         (w_privileges required k0)) m0)
	    end

	    val pickle = pickle_group ()
	    val sz = size pickle
	    val offset_adjustment = sz + 4

	    fun mkStableGroup sname = let
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
			    val share = SmlInfo.share smlinfo
			    val locs = SmlInfo.errorLocation gp smlinfo
			    val error = EM.errorNoSource grpSrcInfo locs
			    val i = BinInfo.new { group = grouppath,
						  stablename = sname,
						  spec = spec,
						  offset = offset,
						  share = share,
						  error = error }
			    val n = DG.BNODE { bininfo = i,
					       localimports = li,
					       globalimports = gi }
			in
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
	    val sname = FilenamePolicy.mkStableName policy gpath
	    fun work outs =
		(Say.vsay ["[stabilizing ", SrcPath.descr gpath, "]\n"];
		 writeInt32 (outs, sz);
		 BinIO.output (outs, Byte.stringToBytes pickle);
		 app (cpb outs) memberlist;
		 mkStableGroup sname)
	in
	    SOME (SafeIO.perform { openIt = fn () => AutoDir.openBinOut sname,
				   closeIt = BinIO.closeOut,
				   work = work,
				   cleanup = fn () =>
				    (OS.FileSys.remove sname handle _ => ()) })
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
		    fun notStable (_, GG.GROUP { kind, ... }) =
			case kind of GG.STABLELIB _ => false | _ => true
		in
		    case List.filter notStable (#sublibs grec) of
			[] => doit wrapped
		      | l => let
			    val grammar = case l of [_] => " is" | _ => "s are"
			    fun ppb pps = let
				fun loop [] = ()
				  | loop ((p, GG.GROUP { grouppath, ... })
					  :: t) =
				    (PP.add_string pps
				        (SrcPath.descr grouppath);
				     PP.add_string pps " (";
				     PP.add_string pps (SrcPath.descr p);
				     PP.add_string pps ")";
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
	    EM.COMPLAIN (concat (gdescr :: ": " :: l)) EM.nullErrorBody

	exception Format

	val pcmode = #pcmode (#param gp)
	val policy = #fnpolicy (#param gp)
	val primconf = #primconf (#param gp)
	val sname = FilenamePolicy.mkStableName policy group
	val _ = Say.vsay ["[checking stable ", gdescr, "]\n"]

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
	    val pickle = bytesIn sz
	    val offset_adjustment = sz + 4

	    val rd = let
		val pos = ref 0
		fun rd () = let
		    val p = !pos
		in
		    pos := p + 1;
		    Byte.byteToChar (Word8Vector.sub (pickle, p))
		    handle _ => raise Format
		end
	    in
		rd
	    end

	    fun r_list r () =
		case rd () of
		    #"0" => []
		  | #"1" => [r ()]
		  | #"2" => [r (), r ()]
		  | #"3" => [r (), r (), r ()]
		  | #"4" => [r (), r (), r (), r ()]
		  | #"5" => r () :: r () :: r () :: r () :: r () :: r_list r ()
		  | _ => raise Format

	    fun r_bool () =
		case rd () of
		    #"t" => true
		  | #"f" => false
		  | _ => raise Format

	    fun r_option r_item () =
		case rd () of
		    #"n" => NONE
		  | #"s" => SOME (r_item ())
		  | _ => raise Format

	    fun r_int () = let
		fun loop n = let
		    val w8 = Byte.charToByte (rd ())
		    val n' =
			n * 0w128 + Word8.toLargeWord (Word8.andb (w8, 0w127))
		in
		    if Word8.andb (w8, 0w128) = 0w0 then n' else loop n'
		end
	    in
		LargeWord.toIntX (loop 0w0)
	    end

	    fun r_share r_raw C unC () =
		case rd () of
		    #"o" => (case IntBinaryMap.find (!m, r_int ()) of
				 SOME x => unC x
			       | NONE => raise Format)
		  | #"n" => let
			val i = !next
			val _ = next := i + 1
			val v = r_raw ()
		    in
			m := IntBinaryMap.insert (!m, i, C v);
			v
		    end
		  | _ => raise Format

	    fun r_string () = let
		fun loop l =
		    case rd () of
			#"\"" => String.implode (rev l)
	              | #"\\" => loop (rd () :: l)
		      | c => loop (c :: l)
	    in
		loop []
	    end

	    fun r_abspath () =
		case SrcPath.unpickle pcmode (r_list r_string (), group) of
		    SOME p => p
		  | NONE => raise Format

	    val r_symbol = let
		fun r_symbol_raw () = let
		    val (ns, first) =
			case rd () of
			    #"'" => (Symbol.sigSymbol, rd ())
			  | #"(" => (Symbol.fctSymbol, rd ())
			  | #")" => (Symbol.fsigSymbol, rd ())
			  | c => (Symbol.strSymbol, c)
		    fun loop (#".", l) = String.implode (rev l)
		      | loop (c, l) = loop (rd (), c :: l)
		in
		    ns (loop (first, []))
		end
		fun unUS (US x) = x
		  | unUS _ = raise Format
	    in
		r_share r_symbol_raw US unUS
	    end

	    val r_ss = let
		fun r_ss_raw () =
		    SymbolSet.addList (SymbolSet.empty, r_list r_symbol ())
		fun unUSS (USS s) = s
		  | unUSS _ = raise Format
	    in
		r_share r_ss_raw USS unUSS
	    end

	    val r_filter = r_option r_ss

	    fun r_primitive () =
		case Primitive.fromIdent primconf (rd ()) of
		    NONE => raise Format
		  | SOME p => p

	    fun r_sharing () =
		case rd () of
		    #"n" => NONE
		  | #"t" => SOME true
		  | #"f" => SOME false
		  | _ => raise Format

	    fun r_si () = let
		val spec = r_string ()
		val locs = r_string ()
		val offset = r_int () + offset_adjustment
		val share = r_sharing ()
		val error = EM.errorNoSource grpSrcInfo locs
	    in
		BinInfo.new { group = group,
			      stablename = sname,
			      error = error,
			      spec = spec,
			      offset = offset,
			      share = share }
	    end

	    fun r_sg () = let
		val p = r_abspath ()
	    in
		(p, getGroup' p)
	    end

	    val sublibs = r_list r_sg ()

	    fun r_bn () =
		case rd () of
		    #"p" => DG.PNODE (r_primitive ())
		  | #"b" => let
			val n = r_int ()
			val sy = r_symbol ()
			val (_, GG.GROUP { exports = slexp, ... }) =
			    List.nth (sublibs, n) handle _ => raise Format
		    in
			case SymbolMap.find (slexp, sy) of
			    SOME ((_, DG.SB_BNODE (n as DG.BNODE _)), _) => n
			  | _ => raise Format
		    end
		  | _ => raise Format

	    fun r_pid () = Pid.fromBytes (Byte.stringToBytes (r_string ()))

	    (* this is the place where what used to be an
	     * SNODE changes to a BNODE! *)
	    fun r_sn_raw () = let
		val popt = r_option r_pid ()
		val i = r_si ()
		val n = DG.BNODE { bininfo = i,
				   localimports = r_list r_sn (),
				   globalimports = r_list r_fsbn () }
	    in
		case popt of
		    NONE => n
		  | SOME p => (warmup (i, p); n)
	    end

	    and r_sn () =
		r_share r_sn_raw UBN (fn (UBN n) => n | _ => raise Format) ()

	    (* this one changes from farsbnode to plain farbnode *)
	    and r_sbn () =
		case rd () of
		    #"b" => r_bn ()
		  | #"s" => r_sn ()
		  | _ => raise Format

	    and r_fsbn () = (r_filter (), r_sbn ())

	    fun r_impexp () = let
		val sy = r_symbol ()
		val (f, n) = r_fsbn ()	(* really reads farbnodes! *)
		val e = bn2env n
		(* put a filter in front to avoid having the FCTENV being
		 * queried needlessly (this avoids spurious module loadings) *)
		val e' = DAEnv.FILTER (SymbolSet.singleton sy, e)
	    in
		(sy, ((f, DG.SB_BNODE n), e')) (* coerce to farsbnodes *)
	    end

	    fun r_exports () =
		foldl SymbolMap.insert' SymbolMap.empty (r_list r_impexp ())

	    fun r_privileges () =
		StringSet.addList (StringSet.empty, r_list r_string ())

	    val exports = r_exports ()
	    val required = r_privileges ()
	    val simap = genStableInfoMap (exports, group)
	in
	    GG.GROUP { exports = exports,
		       kind = GG.STABLELIB simap,
		       required = required,
		       grouppath = group,
		       sublibs = sublibs }
	end
    in
	SOME (SafeIO.perform { openIt = fn () => BinIO.openIn sname,
			       closeIt = BinIO.closeIn,
			       work = work,
			       cleanup = fn () => () })
	handle Format => NONE
             | IO.Io _ => NONE
    end
end

end (* local *)
