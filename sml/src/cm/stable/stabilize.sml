structure Stablize = struct

    structure DG = DependencyGraph
    structure GG = GroupGraph
    structure EM = GenericVC.ErrorMsg

    datatype item =
	SS of SymbolSet.set
      | S of Symbol.symbol
      | SI of SmlInfo.info		(* only used during pickling *)
      | AP of AbsPath.t
      | BI of BinInfo.info		(* only used during unpickling *)

    fun compare (S s, S s') = SymbolOrdKey.compare (s, s')
      | compare (S _, _) = GREATER
      | compare (_, S _) = LESS
      | compare (SS s, SS s') = SymbolSet.compare (s, s')
      | compare (SS _, _) = GREATER
      | compare (_, SS _) = LESS
      | compare (SI i, SI i') = SmlInfo.compare (i, i')
      | compare (SI _, _) = GREATER
      | compare (_, SI _) = LESS
      | compare (AP p, AP p') = AbsPath.compare (p, p')
      | compare (AP _, _) = GREATER
      | compare (_, AP _) = LESS
      | compare (BI i, BI i') = BinInfo.compare (i, i')

    structure Map =
	BinaryMapFn (struct
			 type ord_key = item
			 val compare = compare
	end)

    fun stabilize (g as GG.GROUP grec, binSizeOf, binCopy, gp) =
	case #stableinfo grec of
	    GG.STABLE _ => g
	  | GG.NONSTABLE granted => let

		val exports = #exports grec

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
		val members = let
		    fun sn (DG.SNODE { smlinfo, localimports = l, ... }, s) =
			      if SmlInfoSet.member (s, smlinfo) then s
			      else foldl sn (SmlInfoSet.add (s, smlinfo)) l
		    fun impexp (((_, DG.SB_BNODE _), _), s) = s
		      | impexp (((_, DG.SB_SNODE n), _), s) = sn (n, s)
		in
			      SmlInfoSet.listItems
			      (SymbolMap.foldl impexp SmlInfoSet.empty exports)
		end

		val offsetDict = let
		    fun add (i, (d, n)) =
			(SmlInfoMap.insert (d, i, n), n + binSizeOf i)
		in
		    #1 (foldl add (SmlInfoMap.empty, 0) members)
		end

		fun w_list w_item [] k m = "0" :: k m
		  | w_list w_item [a] k m = "1" :: w_item a k m
		  | w_list w_item [a, b] k m = "2" :: w_item a (w_item b k) m
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

		val w_symbol = w_share w_symbol_raw S

		val w_ss = w_share (w_list w_symbol o SymbolSet.listItems) SS

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

		fun w_si_raw i k = let
		    val spec = AbsPath.spec (SmlInfo.sourcepath i)
		    val locs = SmlInfo.errorLocation gp i
		    val offset = valOf (SmlInfoMap.find (offsetDict, i))
		in
		    w_string spec
		        (w_string locs
		            (w_int offset
			         (w_sharing (SmlInfo.share i) k)))
		end

		val w_si = w_share w_si_raw SI

		fun w_primitive p k m = String.str (Primitive.toIdent p) :: k m

		fun w_abspath_raw p k m =
		    w_list w_string (AbsPath.pickle p) k m

		val w_abspath = w_share w_abspath_raw AP

		fun w_bn (DG.PNODE p) k m = "p" :: w_primitive p k m
		  | w_bn (DG.BNODE { bininfo = i, ... }) k m =
		    "b" :: w_abspath (BinInfo.group i)
		              (w_int (BinInfo.offset i) k) m

		fun w_sn (DG.SNODE n) k =
		    w_si (#smlinfo n)
		        (w_list w_sn (#localimports n)
		              (w_list w_fsbn (#globalimports n) k))

		and w_sbn (DG.SB_BNODE n) k m = "b" :: w_bn n k m
		  | w_sbn (DG.SB_SNODE n) k m = "s" :: w_sn n k m

		and w_fsbn (f, n) k = w_filter f (w_sbn n k)

		fun w_impexp (s, (n, _)) k = w_symbol s (w_fsbn n k)

		fun w_exports e = w_list w_impexp (SymbolMap.listItemsi e)

		fun w_bool true k m = "t" :: k m
		  | w_bool false k m = "f" :: k m

		fun w_privileges p = w_list w_string (StringSet.listItems p)

		fun pickle_group (GG.GROUP g, granted) = let
		    fun w_sg (GG.GROUP g) = w_abspath (#grouppath g)
		    val req' = StringSet.difference (#required g, granted)
		    fun k0 m = []
		    val m0 = (0, Map.empty)
		in
		    concat
		       (w_exports (#exports g)
		          (w_bool (#islib g)
		              (w_privileges req'
			           (w_abspath (#grouppath g)
				         (w_list w_sg (#subgroups g) k0)))) m0)
		end
		val pickle = pickle_group (g, granted)
		val sz = size pickle
	    in
		Dummy.f ()
	    end

    fun g (getGroup, fsbn2env, knownStable, grpSrcInfo, group, s) = let

	exception Format

	(* for getting sharing right... *)
	val m = ref IntBinaryMap.empty
	val next = ref 0

	(* to build the stable info *)
	val simap = ref IntBinaryMap.empty

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
	    fun unAP (AP x) = x
	      | unAP _ = raise Format
	in
	    r_share r_abspath_raw AP unAP
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
	    fun unS (S x) = x
	      | unS _ = raise Format
	in
	    r_share r_symbol_raw S unS
	end

	val r_ss = let
	    fun r_ss_raw () =
		SymbolSet.addList (SymbolSet.empty, r_list r_symbol ())
	    fun unSS (SS s) = s
	      | unSS _ = raise Format
	in
	    r_share r_ss_raw SS unSS
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

	val r_si = let
	    fun r_si_raw () = let
		val spec = r_string ()
		val locs = r_string ()
		val offset = r_int () + offset_adjustment
		val share = r_sharing ()
		val error = EM.errorNoSource grpSrcInfo locs
		val i = BinInfo.new { group = group,
				      error = error,
				      spec = spec,
				      offset = offset,
				      share = share }
	    in
		simap := IntBinaryMap.insert (!simap, offset, i);
		i
	    end
	    fun unBI (BI i) = i
	      | unBI _ = raise Format
	in
	    r_share r_si_raw BI unBI
	end

	fun r_bn () =
	    case rd () of
		#"p" => DG.PNODE (r_primitive ())
	      | #"b" =>
		    (case AbsPathMap.find (knownStable, r_abspath ()) of
			 NONE => raise Format
		       | SOME im =>
			     (case IntBinaryMap.find (im, r_int ()) of
				  NONE => raise Format
				| SOME n => n))
	      | _ => raise Format

	(* this is the place where what used to be an
	 * SNODE changes to a BNODE! *)
	fun r_sn () =
	    DG.BNODE { bininfo = r_si (),
		       localimports = r_list r_sn (),
		       globalimports = r_list r_fsbn () }

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
	    val e = fsbn2env n
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
	    val grouppath = r_abspath ()
	    val subgroups = r_list (getGroup o r_abspath) ()
	    fun add (((_, DG.SB_BNODE (DG.BNODE { bininfo, ... })), _), s) =
		IntBinarySet.add (s, BinInfo.offset bininfo)
	      | add (_, s) = s
	    val ens = SymbolMap.foldl add IntBinarySet.empty exports
	    fun isExported (os, _) = IntBinarySet.member (ens, os)
	    val final_simap = IntBinaryMap.filteri isExported (!simap)
	in
	    GG.GROUP { exports = exports,
		       islib = islib,
		       required = required,
		       grouppath = grouppath,
		       subgroups = subgroups,
		       stableinfo = GG.STABLE final_simap }
	end
    in
	SOME (unpickle_group ()) handle Format => NONE
    end
end
