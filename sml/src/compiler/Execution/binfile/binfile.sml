(* binfile-new.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *
 * author: Matthias Blume (blume@research.bell-labs.com
 *)

(*
 * This revised version of structure Binfile is now machine-independent.
 * Moreover, it deals with the file format only and does not know how to
 * create new binfile contents (aka "compile") or how to interpret the
 * pickles.  As a result, it does not statically depend on the compiler.
 * (Eventually we might want to support a light-weight binfile loader.)
 * 
 * ----------------------------------------------------------------------------
 * BINFILE FORMAT description:
 *
 *  Every 4-byte integer field is stored in big-endian format.
 *
 *        Start Size Purpose
 * ----BEGIN OF HEADER----
 *            0 16  magic string
 *           16  4  number of import values (importCnt)
 *           20  4  number of exports (exportCnt = currently always 0 or 1)
 *           24  4  size of import tree area in bytes (importSzB)
 *           28  4  size of CM-specific info in bytes (cmInfoSzB)
 *           32  4  size of pickled lambda-expression in bytes (lambdaSzB)
 *           36  4  size of GUID area in bytes (g)
 *           40  4  size of padding in bytes (pad)
 *           44  4  size of code area in bytes (codeSzB)
 *           48  4  size of pickled environment in bytes (envSzB)
 *           52  i  import trees [This area contains pickled import trees --
 *                    see below.  The total number of leaves in these trees is
 *                    importCnt and the size is equal to importSzB.]
 *         i+52 ex  export pids [Each export pid occupies 16 bytes. Thus, the
 *                    size ex of this area is 16*exportCnt (0 or 16).]
 *      ex+i+52 cm  CM info [Currently a list of pid-pairs.] (cm = cmInfoSzB)
 * ----END OF HEADER----
 *            0  h  HEADER (h = 52+cm+ex+i)
 *            h  l  pickle of exported lambda-expr. (l = lambdaSzB)
 *          l+h  g  GUID area (g = guid)
 *        r+l+h  p  padding (p = pad)
 *      p+r+l+h  c  code area (c = codeSzB) [Structured into several
 *                    segments -- see below.]
 *    c+p+r+l+h  e  pickle of static environment (e = envSzB)
 *  e+c+p+r+l+h  -  END OF BINFILE
 *
 * IMPORT TREE FORMAT description:
 *
 *  The import tree area contains a list of (pid * tree) pairs.
 *  The pids are stored directly as 16-byte strings.
 *  Trees are constructed according to the following ML-datatype:
 *    datatype tree = NODE of (int * tree) list
 *  Leaves in this tree have the form (NODE []).
 *  Trees are written recursively -- (NODE l) is represented by n (= the
 *  length of l) followed by n (int * node) subcomponents.  Each component
 *  consists of the integer selector followed by the corresponding tree.
 *
 *  Integer values in the import tree area (lengths and selectors) are
 *  written in "packed" integer format. In particular, this means that
 *  Values in the range 0..127 are represented by only 1 byte.
 *  Conceptually, the following pickling routine is used:
 *
 *    void recur_write_ul (unsigned long l, FILE *file)
 *    {
 *      if (l != 0) {
 *        recur_write_ul (l >> 7, file);
 *        putc ((l & 0x7f) | 0x80, file);
 *      }
 *    }
 *
 *    void write_ul (unsigned long l, FILE *file)
 *    {
 *      recur_write_ul (l >> 7, file);
 *      putc (l & 0x7f, file);
 *    }
 *
 * CODE AREA FORMAT description:
 *
 *  The code area contains multiple code segements.  There will be at least
 *  two.  The very first segment is the "data" segment -- responsible for
 *  creating literal constants on the heap.  The idea is that code in the
 *  data segment will be executed only once at link-time. Thus, it can
 *  then be garbage-collected immediatly. (In fact, the data segment does
 *  not consist of machine code but of code for an internal bytecode engine.)
 *
 *  In the binfile, each code segment is represented by its size s and its
 *  entry point offset (in bytes -- written as 4-byte big-endian integers)
 *  followed by s bytes of machine- (or byte-) code. The total length of all
 *  code segments (including the bytes spent on representing individual sizes
 *  and entry points) is codeSzB.  The entrypoint field for the data segment
 *  is currently ignored (and should be 0).
 *
 * LINKING CONVENTIONS:
 *
 *  Linking is achieved by executing all code segments in sequential order.
 *
 *  Conceptually, the first code segment (i.e., the "data" segment) receives
 *  unit as its single argument. (In reality this code segment consists of
 *  bytecode which does not really receive any arguments.)
 *
 *  The second code segment receives a record as its single argument.
 *  This record has (importCnt+1) components.  The first importCnt
 *  components correspond to the leaves of the import trees.  The final
 *  component is the result from executing the data segment.
 *
 *  All other code segments receive a single argument which is the result
 *  of the preceding segment.
 *
 *  The result of the last segment represents the exports of the compilation
 *  unit.  It is to be paired up with the export pid and stored in the
 *  dynamic environment.  If there is no export pid, then the final result
 *  will be thrown away.
 *
 *  The import trees are used for constructing the argument record for the
 *  second code segment.  The pid at the root of each tree is the key for
 *  looking up a value in the existing dynamic environment.  In general,
 *  that value will be a record.  The selector fields of the import tree
 *  associated with the pid are used to recursively fetch components of that
 *  record.
 *)
structure Binfile :> BINFILE = struct

    structure Pid = PersStamps

    exception FormatError = CodeObj.FormatError

    type pid = Pid.persstamp

    type csegments = CodeObj.csegments

    type executable = CodeObj.executable

    type stats = { env: int, inlinfo: int, data: int, code: int }

    type pickle = { pid: pid, pickle: Word8Vector.vector }

    datatype bfContents =
	BF of { imports: ImportTree.import list,
		exportPid: pid option,
		cmData: pid list,
		senv: pickle,
		lambda: pickle,
		guid: string,
		csegments: csegments,
		executable: executable option ref }
    fun unBF (BF x) = x

    val bytesPerPid = Pid.persStampSize
    val magicBytes = 16

    val exportPidOf = #exportPid o unBF
    val cmDataOf = #cmData o unBF
    val senvPickleOf = #senv o unBF
    val staticPidOf = #pid o senvPickleOf
    val lambdaPickleOf = #lambda o unBF
    val lambdaPidOf = #pid o lambdaPickleOf

    val guidOf = #guid o unBF

    fun error msg =
	(Control_Print.say (concat ["binfile format error: ", msg, "\n"]);
	 raise FormatError)

    val fromInt = Word32.fromInt
    val fromByte = Word32.fromLargeWord o Word8.toLargeWord
    val toByte = Word8.fromLargeWord o Word32.toLargeWord
    val >> = Word32.>>
    infix >>

    fun bytesIn (s, 0) = Byte.stringToBytes ""
      | bytesIn (s, n) = let
	    val bv = BinIO.inputN (s, n)
	in
	    if n = Word8Vector.length bv then bv
	    else error (concat["expected ", Int.toString n,
			       " bytes, but found ",
			       Int.toString(Word8Vector.length bv)])
	end

    fun readInt32 s = LargeWord.toIntX(Pack32Big.subVec(bytesIn(s, 4), 0))

    fun readPackedInt32 s = let
	fun loop n =
	    case BinIO.input1 s of
		NONE => error "unable to read a packed int32"
	      | SOME w8 => let
		    val n' =
			n * 0w128
			+ Word8.toLargeWord (Word8.andb (w8, 0w127))
		in
		    if Word8.andb (w8, 0w128) = 0w0 then n' else loop n'
		end
    in
	LargeWord.toIntX (loop 0w0)
    end

    fun readPid s = Pid.fromBytes (bytesIn (s, bytesPerPid))
    fun readPidList (s, n) = List.tabulate (n, fn _ => readPid s)

    fun readImportTree s =
	case readPackedInt32 s of
	    0 =>  (ImportTree.ITNODE [], 1)
	  | cnt => let
		fun readImportList 0 = ([], 0)
		  | readImportList cnt = let
			val selector = readPackedInt32 s
			val (tree, n) = readImportTree s
			val (rest, n') = readImportList (cnt - 1)
		    in
			((selector, tree) :: rest, n + n')
		    end
		val (l, n) = readImportList cnt
	    in
		(ImportTree.ITNODE l, n)
	    end

    fun readImports (s, n) =
	if n <= 0 then []
	else let
		val pid = readPid s
		val (tree, n') = readImportTree s
		val rest = readImports (s, n - n')
	    in
		(pid, tree) :: rest
	    end

    fun pickleInt32 i = let
	val w = fromInt i
	fun out w = toByte w
    in
	Word8Vector.fromList [toByte (w >> 0w24), toByte (w >> 0w16),
			      toByte (w >> 0w8), toByte w]
    end
    fun writeInt32 s i = BinIO.output (s, pickleInt32 i)

    fun picklePackedInt32 i = let
	val n = fromInt i
	val // = LargeWord.div
	val %% = LargeWord.mod
	val !! = LargeWord.orb
	infix // %% !!
	val toW8 = Word8.fromLargeWord
	fun r (0w0, l) = Word8Vector.fromList l
	  | r (n, l) = r (n // 0w128, toW8 ((n %% 0w128) !! 0w128) :: l)
    in
	r (n // 0w128, [toW8 (n %% 0w128)])
    end

    fun writePid (s, pid) = BinIO.output (s, Pid.toBytes pid)
    fun writePidList (s, l) = app (fn p => writePid (s, p)) l

    local
	fun pickleImportSpec ((selector, tree), (n, p)) = let
	    val sp = picklePackedInt32 selector
	    val (n', p') = pickleImportTree (tree, (n, p))
	in
	    (n', sp :: p')
	end
	and pickleImportTree (ImportTree.ITNODE [], (n, p)) =
	    (n + 1, picklePackedInt32 0 :: p)
	  | pickleImportTree (ImportTree.ITNODE l, (n, p)) = let
		val (n', p') = foldr pickleImportSpec (n, p) l
	    in
		(n', picklePackedInt32 (length l) :: p')
	    end

	fun pickleImport ((pid, tree), (n, p)) = let
	    val (n', p') = pickleImportTree (tree, (n, p))
	in
	    (n', Pid.toBytes pid :: p')
	end
    in
        fun pickleImports l = let
	    val (n, p) = foldr pickleImport (0, []) l
	in
	    (n, Word8Vector.concat p)
	end
    end

    fun mkMAGIC (arch, version_id) = let
	val vbytes = 8			(* version part *)
	val abytes = magicBytes - vbytes - 1 (* arch part *)
	fun fit (i, s) = let
	    val s = StringCvt.padRight #" " i s
	in
	    if (size s = i) then s
	    else substring (s, 0, i)
	end
	fun version [] = []
	  | version [x : int] = [Int.toString x]
	  | version (x :: r) = (Int.toString x) :: "." :: (version r)
	val v = fit (vbytes, concat (version version_id))
	val a = fit (abytes, arch)
    in
	concat [v, a, "\n"]
	(* assert (Word8Vector.length (MAGIC <arch>) = magicBytes *)
    end

    (* calculate size of code objects (including lengths and entrypoints) *)
    fun codeSize (csegs: csegments) =
	List.foldl
	    (fn (co, n) => n + CodeObj.size co + 8)
	    (CodeObj.size(#c0 csegs) + Word8Vector.length(#data csegs) + 16)
	    (#cn csegs)

    (* This function must be kept in sync with the "write" function below.
     * It calculates the number of bytes written by a corresponding
     * call to "write". *)
    fun size { contents, nopickle } = let
	val { imports, exportPid, senv, cmData, lambda,  csegments,
	      guid, ... } =
	    unBF contents
	val (_, picki) = pickleImports imports
	val hasExports = isSome exportPid
	fun pickleSize { pid, pickle } =
	    if nopickle then 0 else Word8Vector.length pickle
    in
	magicBytes +
	9 * 4 +
	Word8Vector.length picki +
	(if hasExports then bytesPerPid else 0) +
	bytesPerPid * (length cmData + 2) + (* 2 extra: stat/sym *)
	String.size guid +
	pickleSize lambda +
	codeSize csegments +
	pickleSize senv
    end

    fun create { imports, exportPid, cmData, senv, lambda, csegments, guid } =
	BF { imports = imports,
	     exportPid = exportPid,
	     cmData = cmData,
	     senv = senv,
	     lambda = lambda,
	     guid = guid,
	     csegments = csegments,
	     executable = ref NONE }

    (* must be called with second arg >= 0 *)
    fun readCodeList (strm, nbytes) = let
	fun readCode 0 = []
	  | readCode n = let
		val sz = readInt32 strm
		val ep = readInt32 strm
		val n' = n - sz - 8
		val cobj = if n' < 0 then error "code size"
			   else CodeObj.input(strm, sz)
	    in
		CodeObj.set_entrypoint (cobj, ep);
		cobj :: readCode n'
	    end
	val dataSz = readInt32 strm
	val _ = readInt32 strm (* ignore entry point field for data segment *)
	val n' = nbytes - dataSz - 8
	val data = if n' < 0 then error "data size" else bytesIn (strm, dataSz)
    in
	case readCode n' of
	    (c0 :: cn) => { data = data, c0 = c0, cn = cn }
	  | [] => error "missing code objects"
    end

    fun readGUid s = let
	val _ = bytesIn (s, magicBytes)
	val _ = readInt32 s
	val ne = readInt32 s
	val importSzB = readInt32 s
	val cmInfoSzB = readInt32 s
	val nei = cmInfoSzB div bytesPerPid
	val lambdaSz = readInt32 s
	val g = readInt32 s
	val _ = bytesIn (s, importSzB + 3 * 4)
	val _ = bytesIn (s, ne * bytesPerPid)
	val _ = readPidList (s, nei)
	val _ = bytesIn (s, lambdaSz)
    in
	Byte.bytesToString (bytesIn (s, g))
    end

    fun read { arch, version, stream = s } = let
	val MAGIC = mkMAGIC (arch, version)
	val magic = bytesIn (s, magicBytes)
	val _ = if Byte.bytesToString magic = MAGIC then ()
		else error "bad magic number"
	val leni = readInt32 s
	val ne = readInt32 s
	val importSzB = readInt32 s
	val cmInfoSzB = readInt32 s
	val nei = cmInfoSzB div bytesPerPid
	val lambdaSz = readInt32 s
	val g = readInt32 s
	val pad = readInt32 s
	val cs = readInt32 s
	val es = readInt32 s
	val imports = readImports (s, leni)
	val exportPid =
	    (case ne of
		 0 => NONE
	       | 1 => SOME(readPid s)
	       | _ => error "too many export PIDs")
	val envPids = readPidList (s, nei)
	val (staticPid, lambdaPid, cmData) =
	    case envPids of
		st :: lm :: cmData => (st, lm, cmData)
	      | _ => error "env PID list"
	val plambda = bytesIn (s, lambdaSz)
	val guid = Byte.bytesToString (bytesIn (s, g))
	(* skip padding *)
	val _ = if pad <> 0 then ignore (bytesIn (s, pad)) else ()
	(* now get the code *)
	val code = readCodeList (s, cs)
	val penv = bytesIn (s, es)
    in
	{ contents = create { imports = imports,
			      exportPid = exportPid,
			      cmData = cmData,
			      senv = { pid = staticPid, pickle = penv },
			      lambda = { pid = lambdaPid, pickle = plambda },
			      guid = guid,
			      csegments = code },
	  stats = { env = es, inlinfo = lambdaSz, code = cs,
		    data = Word8Vector.length (#data code) } }
    end

    fun write { arch, version, stream = s, contents, nopickle } = let
	(* Keep this in sync with "size" (see above). *)
	val { imports, exportPid, cmData, senv, lambda,
	      csegments, guid, ... } =
	    unBF contents
	val { pickle = senvP, pid = staticPid } = senv
	val { pickle = lambdaP, pid = lambdaPid } = lambda
	val envPids = staticPid :: lambdaPid :: cmData
	val (leni, picki) = pickleImports imports
	val importSzB = Word8Vector.length picki
	val (ne, epl) =
	    case exportPid of
		NONE => (0, [])
	      | SOME p => (1, [p])
	val nei = length envPids
	val cmInfoSzB = nei * bytesPerPid
	fun pickleSize { pid, pickle } =
	    if nopickle then 0 else Word8Vector.length pickle
	val lambdaSz = pickleSize lambda
	val g = String.size guid
	val pad = 0			(* currently no padding *)
	val cs = codeSize csegments
	fun codeOut c = (writeInt32 s (CodeObj.size c);
			 writeInt32 s (CodeObj.entrypoint c);
			 CodeObj.output (s, c))
	val es = pickleSize senv
	val writeEnv = if nopickle then fn () => ()
		       else fn () => BinIO.output (s, senvP)
	val datasz = Word8Vector.length (#data csegments)
	val MAGIC = mkMAGIC (arch, version)
    in
	BinIO.output (s, Byte.stringToBytes MAGIC);
	app (writeInt32 s) [leni, ne, importSzB, cmInfoSzB,
			    lambdaSz, g, pad, cs, es];
	BinIO.output (s, picki);
	writePidList (s, epl);
	(* arena1 *)
	writePidList (s, envPids);
	(* arena2 -- pickled flint stuff *)
	if lambdaSz = 0 then () else BinIO.output (s, lambdaP);
	(* GUID area *)
	BinIO.output (s, Byte.stringToBytes guid);
	(* padding area is currently empty *)
	(* code objects *)
	writeInt32 s datasz;
	writeInt32 s 0;		(* dummy entry point for data segment *)
	BinIO.output(s, #data csegments);
	codeOut (#c0 csegments);
	app codeOut (#cn csegments);
	writeEnv ();
	{ env = es, inlinfo = lambdaSz, data = datasz, code = cs }
    end

    fun exec (BF { imports, exportPid, executable, csegments, ... },
	      dynenv, exnwrapper) =
	let val executable =
		case !executable of
		    SOME e => e
		  | NONE => let
			val e = Isolate.isolate
				    (Execute.mkexec
					 { cs = csegments,
					   exnwrapper = exnwrapper })
		    in executable := SOME e; e
		    end
	in
	    Execute.execute { executable = executable,
			      imports = imports,
			      exportPid = exportPid,
			      dynenv = dynenv }
	end
end
