structure Stablize = struct

    structure DG = DependencyGraph
    structure GG = GroupGraph
    structure EM = GenericVC.ErrorMsg

    datatype pitem =
	PSS of SymbolSet.set
      | PS of Symbol.symbol
      | PSN of DG.snode
      | PAP of AbsPath.t

    datatype uitem =
	USS of SymbolSet.set
      | US of Symbol.symbol
      | UBN of DG.bnode
      | UAP of AbsPath.t

    fun compare (PS s, PS s') = SymbolOrdKey.compare (s, s')
      | compare (PS _, _) = GREATER
      | compare (_, PS _) = LESS
      | compare (PSS s, PSS s') = SymbolSet.compare (s, s')
      | compare (PSS _, _) = GREATER
      | compare (_, PSS _) = LESS
      | compare (PSN (DG.SNODE n), PSN (DG.SNODE n')) =
	SmlInfo.compare (#smlinfo n, #smlinfo n')
      | compare (PSN _, _) = GREATER
      | compare (_, PSN _) = LESS
      | compare (PAP p, PAP p') = AbsPath.compare (p, p')

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
	    if AbsPath.compare (BinInfo.group i, group) = EQUAL then
		IntBinaryMap.insert (m, BinInfo.offset i, n)
	    else m
	end
	  | add (_, m) = m
    in
	SymbolMap.foldl add IntBinaryMap.empty exports
    end

    fun stabilize gp (g as GG.GROUP grec, binSizeOf, copyBin, outs) =
	case #stableinfo grec of
	    GG.STABLE _ => g
	  | GG.NONSTABLE granted => let

		(* this needs to be refined (perhaps) *)
		val grpSrcInfo = (EM.defaultConsumer (), ref false)

		val exports = #exports grec
		val islib = #islib grec
		val required = StringSet.difference (#required grec,
						     granted)
		val grouppath = #grouppath grec
		val subgroups = #subgroups grec

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
		    val spec = AbsPath.spec (SmlInfo.sourcepath i)
		    val locs = SmlInfo.errorLocation gp i
		    val offset = registerOffset (i, binSizeOf i)
		in
		    w_string spec
		        (w_string locs
		            (w_int offset
			         (w_sharing (SmlInfo.share i) k)))
		end

		fun w_primitive p k m = String.str (Primitive.toIdent p) :: k m

		fun w_abspath_raw p k m =
		    w_list w_string (AbsPath.pickle p) k m

		val w_abspath = w_share w_abspath_raw PAP

		fun w_bn (DG.PNODE p) k m = "p" :: w_primitive p k m
		  | w_bn (DG.BNODE { bininfo = i, ... }) k m =
		    "b" :: w_abspath (BinInfo.group i)
		              (w_int (BinInfo.offset i) k) m

		fun w_sn_raw (DG.SNODE n) k =
		    w_si (#smlinfo n)
		        (w_list w_sn (#localimports n)
		              (w_list w_fsbn (#globalimports n) k))

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
		    fun w_sg (GG.GROUP g) = w_abspath (#grouppath g)
		    fun k0 m = []
		    val m0 = (0, Map.empty)
		in
		    concat
		       (w_exports exports
			   (w_bool islib
		              (w_privileges required
			            (w_list w_sg subgroups k0))) m0)
		end

		val pickle = pickle_group ()
		val sz = size pickle
		val offset_adjustment = sz + 4

		fun mkStableGroup () = let
		    val m = ref SmlInfoMap.empty
		    fun sn (DG.SNODE (n as { smlinfo, ... })) =
			case SmlInfoMap.find (!m, smlinfo) of
			    SOME n => n
			  | NONE => let
				val li = map sn (#localimports n)
				val gi = map fsbn (#globalimports n)
				val sourcepath = SmlInfo.sourcepath smlinfo
				val spec = AbsPath.spec sourcepath
				val offset =
				    getOffset smlinfo + offset_adjustment
				val share = SmlInfo.share smlinfo
				val locs = SmlInfo.errorLocation gp smlinfo
				val error = EM.errorNoSource grpSrcInfo locs
				val i = BinInfo.new { group = grouppath,
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
			       islib = islib,
			       required = required,
			       grouppath = grouppath,
			       subgroups = subgroups,
			       stableinfo = GG.STABLE simap }
		end

		fun writeInt32 (s, i) = let
		    val a = Word8Array.array (4, 0w0)
		    val _ = Pack32Big.update (a, 0, LargeWord.fromInt i)
		in
		    BinIO.output (s, Word8Array.extract (a, 0, NONE))
		end
	    in
		writeInt32 (outs, sz);
		BinIO.output (outs, Byte.stringToBytes pickle);
		app (copyBin outs) (rev (!members));
		mkStableGroup ()
	    end

    fun g (getGroup, bn2env, group, s) = let

	(* we don't care about errors... (?) *)
	val grpSrcInfo = (EM.defaultConsumer (), ref false)

	exception Format

	(* for getting sharing right... *)
	val m = ref IntBinaryMap.empty
	val next = ref 0

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
		val n' = n * 0w128 + Word8.toLargeWord (Word8.andb (w8, 0w127))
	    in
		if Word8.andb (w8, 0w128) = 0w0 then n' else loop n'
	    end
	in
	    LargeWord.toIntX (loop 0w0)
	end

	fun r_share r_raw C unC () =
	    case rd () of
		#"o" =>	(case IntBinaryMap.find (!m, r_int ()) of
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

	val r_abspath = let
	    fun r_abspath_raw () =
		case AbsPath.unpickle (r_list r_string ()) of
		    SOME p => p
		  | NONE => raise Format
	    fun unUAP (UAP x) = x
	      | unUAP _ = raise Format
	in
	    r_share r_abspath_raw UAP unUAP
	end

    	val r_symbol = let
	    fun r_symbol_raw () = let
		val (ns, first) =
		    case rd () of
			#"`" => (Symbol.sigSymbol, rd ())
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
	    case Primitive.fromIdent (rd ()) of
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
			  error = error,
			  spec = spec,
			  offset = offset,
			  share = share }
	end

	fun r_bn () =
	    case rd () of
		#"p" => DG.PNODE (r_primitive ())
	      | #"b" => let
		    val p = r_abspath ()
		    val os = r_int ()
		    val GG.GROUP { stableinfo, ... } = getGroup p
		in
		    case stableinfo of
			GG.NONSTABLE _ => raise Format
		      | GG.STABLE im =>
			    (case IntBinaryMap.find (im, os) of
				 NONE => raise Format
			       | SOME n => n)
		end
	      | _ => raise Format

	(* this is the place where what used to be an
	 * SNODE changes to a BNODE! *)
	fun r_sn_raw () =
	    DG.BNODE { bininfo = r_si (),
		       localimports = r_list r_sn (),
		       globalimports = r_list r_fsbn () }

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
	in
	    (sy, ((f, DG.SB_BNODE n), e)) (* coerce to farsbnodes *)
	end

	fun r_exports () =
	    foldl SymbolMap.insert' SymbolMap.empty (r_list r_impexp ())

	fun r_privileges () =
	    StringSet.addList (StringSet.empty, r_list r_string ())

	fun unpickle_group () = let
	    val exports = r_exports ()
	    val islib = r_bool ()
	    val required = r_privileges ()
	    val subgroups = r_list (getGroup o r_abspath) ()
	    val simap = genStableInfoMap (exports, group)
	in
	    GG.GROUP { exports = exports,
		       islib = islib,
		       required = required,
		       grouppath = group,
		       subgroups = subgroups,
		       stableinfo = GG.STABLE simap }
	end
    in
	SOME (unpickle_group ()) handle Format => NONE
    end
end
