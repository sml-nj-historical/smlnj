(* COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies *)
(* binfile.sml *)

functor BinfileFun (C : COMPILE) : BINFILE =
struct
    structure Pid = PersStamps
    structure Env = Environment
    structure Err = ErrorMsg
    structure CB = CompBasic

    exception FormatError = CodeObj.FormatError

    exception Compile = C.Compile
    exception TopLevelException = C.TopLevelException
    exception SilentException = C.SilentException

    fun error msg = (
      Control.Print.say(concat["Bin file format error: ", msg, "\n"]);
      raise FormatError)

    type pid = Pid.persstamp
    type senv = Env.staticEnv
    type symenv = Env.symenv
    type denv = Env.dynenv
    type env = Env.environment
    type lambda = CB.flint

    type csegments = C.csegments
    type executable = C.executable

    fun delay thunk = let
	val f = ref (fn () => raise Fail "Binfile.delay not initialized")
	fun first () = let
	    val r = thunk ()
	    fun later () = r
	in
	    f := later; r
	end
    in
	f := first;
	fn () => !f ()
    end

    (* pickled data: the data, the pid, and the pickle *)
    type 'a pData =
       { unpickled: unit -> 'a, pid: pid, pickled: Word8Vector.vector }

    datatype bfContent =
	BFC of {
		imports: C.import list,
		exportPid: pid option,
		cmData: pid list,
		senv: senv pData,
		lambda: lambda option pData,
		csegments: csegments,
		executable: executable option ref
	       }

    fun staticPidOf (BFC { senv = { pid, ... }, ... }) = pid
    fun exportPidOf (bfc as BFC { exportPid, ... }) = exportPid
    fun lambdaPidOf (BFC { lambda = { pid, ... }, ... }) = pid
    fun cmDataOf (BFC { cmData, ... }) = cmData
    fun senvOf (BFC { senv = { unpickled, ... }, ... }) = unpickled ()
    fun symenvOf (bfc as BFC { senv, lambda, ... }) =
	C.mksymenv (exportPidOf bfc, #unpickled lambda ())

    local
	val arch = C.architecture

	fun executableOf (BFC { executable = ref (SOME e), ... }) = e
	  | executableOf (BFC { executable, csegments, ... }) = let
		val e = C.isolate (C.mkexec csegments)
	    in
		executable := SOME e;
		e
	    end

	fun codeSegments (BFC { csegments, ... }) = csegments

	val fromInt = Word32.fromInt
	val fromByte = Word32.fromLargeWord o Word8.toLargeWord
	val toByte = Word8.fromLargeWord o Word32.toLargeWord
	val >> = Word32.>>
	infix >>

(*
 * BINFILE FORMAT description:
 *
 *  Every 4-byte integer field is stored in big-endian format.
 *
 *     Start Size Purpose
 * ----BEGIN OF HEADER----
 *          0 16  magic string
 *         16  4  number of import values (importCnt)
 *         20  4  number of exports (exportCnt = currently always 0 or 1)
 *         24  4  size of import tree area in bytes (importSzB)
 *         28  4  size of CM-specific info in bytes (cmInfoSzB)
 *         32  4  size of pickled lambda-expression in bytes (lambdaSzB)
 *         36  4  size of reserved area 1 in bytes (reserved1)
 *         40  4  size of reserved area 2 in bytes (reserved2)
 *         44  4  size of code area in bytes (codeSzB)
 *         48  4  size of pickled environment in bytes (envSzB)
 *         52  i  import trees [This area contains pickled import trees --
 *                  see below.  The total number of leaves in these trees is
 *                  importCnt and the size is equal to importSzB.]
 *       i+52 ex  export pids [Each export pid occupies 16 bytes. Thus, the
 *                  size ex of this area is 16*exportCnt (0 or 16).]
 *    ex+i+52 cm  CM info [Currently a list of pid-pairs.] (cm = cmInfoSzB)
 * ----END OF HEADER----
 *          0  h  HEADER (h = 52+cm+ex+i)
 *          h  l  pickle of exported lambda-expr. (l = lambdaSzB)
 *        l+h  r  reserved areas (r = reserved1+reserved2)
 *      r+l+h  c  code area (c = codeSzB) [Structured into several
 *                  segments -- see below.]
 *    c+r+l+h  e  pickle of static environment (e = envSzB)
 *  e+c+r+l+h  -  END OF BINFILE
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
 *  The size of the import tree area is only given implicitly. When reading
 *  this area, the reader must count the number of leaves and compare it
 *  with importCnt.
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
 *  then be garbage-collected immediatly. (In the future it is possible that
 *  the data segment will not contain executable code at all but some form
 *  of bytecode that is to be interpreted separately.)
 *
 *  In the binfile, each code segment is represented by its size s (in
 *  bytes -- written as a 4-byte big-endian integer) followed by s bytes of
 *  machine- (or byte-) code. The total length of all code segments
 *  (including the bytes spent on representing individual sizes) is codeSzB.
 *
 * LINKING CONVENTIONS:
 *
 *  Linking is achieved by executing all code segments in sequential order.
 *
 *  The first code segment (i.e., the "data" segment) receives unit as
 *  its single argument.
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

	val MAGIC = let
	    fun fit (i, s) = let
		val s = StringCvt.padRight #" " i s
	    in
		if (size s = i) then s
		else substring (s, 0, i)
	    end
	    fun version [] = []
	      | version [x : int] = [Int.toString x]
	      | version (x :: r) = (Int.toString x) :: "." :: (version r)
	    val v = fit (8, concat (version (#version_id Version.version)))
	    val a =
		if String.sub (arch, 0) = #"." then
		    fit (7, substring (arch, 1, (String.size arch) - 1))
		else fit (7, arch)
	in
	    Byte.stringToBytes (concat [v, a, "\n"])
	end

	val magicBytes = Word8Vector.length MAGIC
	val bytesPerPid = 16

	fun bytesIn (s, n) = let
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
		0 =>  (CB.ITNODE [], 1)
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
		    (CB.ITNODE l, n)
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
	    and pickleImportTree (CB.ITNODE [], (n, p)) =
		(n + 1, picklePackedInt32 0 :: p)
	      | pickleImportTree (CB.ITNODE l, (n, p)) = let
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

	fun checkMagic s =
	    if bytesIn (s, magicBytes) = MAGIC then ()
	    else error "bad magic number"

	fun readHeader s = let
	    val _ = checkMagic s
	    val leni = readInt32 s
	    val ne = readInt32 s
	    val importSzB = readInt32 s
	    val cmInfoSzB = readInt32 s
	    val nei = cmInfoSzB div bytesPerPid
	    val sLam = readInt32 s
	    val sa1 = readInt32 s
	    val sa2 = readInt32 s
	    val cs = readInt32 s
	    val es = readInt32 s
	    val imports = readImports (s, leni)
	    val exportPid =
		(case ne of
		     0 => NONE
		   | 1 => SOME(readPid s)
		   | _ => error "too many export PIDs")
	    val envPids = readPidList (s, nei)
	in
	    case envPids of
		st :: lm :: cmData =>
		    { nExports = ne,
		      lambdaSz = sLam,
		      res1Sz = sa1, res2Sz = sa2, codeSz = cs, envSz = es,
		      imports = imports, exportPid = exportPid,
		      cmData = cmData, staticPid = st, lambdaPid = lm }
	      | _ => error "env PID list"
	end

      (* must be called with second arg >= 0 *)
	fun readCodeList (strm, name, nbytes) = let
	      fun readCode 0 = []
		| readCode n = let
		    val sz = readInt32 strm
		    val n' = n - sz - 4
		    in
		      if n' < 0
			then error "code size"
			else CodeObj.input(strm, sz, SOME name) :: readCode n'
		    end
	      val dataSz = readInt32 strm
	      val n' = nbytes - dataSz - 4
	      val data = if n' < 0 then error "data size" else bytesIn (strm, dataSz)
	      in
		case readCode n'
		 of (c0 :: cn) => {data = data, c0 = c0, cn = cn}
		  | [] => error "missing code objects"
		(* end case *)
	      end
    in
	fun read args = let
	    val { name, stream = s, modmap = m } = args
	    fun context _ = m
	    val { nExports = ne, lambdaSz = sa2,
		  res1Sz, res2Sz, codeSz = cs, envSz = es,
		  imports, exportPid, cmData,
		  staticPid, lambdaPid } = readHeader s
	    val (lambda_i, plambda) =
		if sa2 = 0 then (fn () => NONE,
				 Word8Vector.fromList [])
		else let
		    val bytes = bytesIn (s, sa2)
		in
		    (delay (fn () => UnpickMod.unpickleFLINT bytes),
		     bytes)
		end
	    val _ = if res1Sz = 0 andalso res2Sz = 0
			then () else error "non-zero reserved size"
	    val code = readCodeList (s, name, cs)
	    val (senv, penv) =
		if es = 0 then (fn () => StaticEnv.empty,
				Word8Vector.fromList [])
		else let
		    val penv = bytesIn (s, es)
		    val _ =
			if Word8Vector.length penv = es then ()
			else error "missing bytes in bin file"
		    val mkSenv =
			delay (fn () => UnpickMod.unpickleEnv
					    context (staticPid, penv))
		in
		    (mkSenv, penv)
		end
	    fun pd (u, p, pk) = { unpickled = u, pid = p, pickled = pk }
        in
	    BFC { imports = imports,
		  exportPid = exportPid,
		  cmData = cmData,
		  senv = pd (senv, staticPid, penv),
		  lambda = pd (lambda_i, lambdaPid, plambda),
		  csegments = code,
		  executable = ref NONE }
        end

	(* compute size of code objects (including length fields) *)
	fun codeSize (csegs: csegments) =
	    List.foldl
	    (fn (co, n) => n + CodeObj.size co + 4)
	      (CodeObj.size(#c0 csegs) + Word8Vector.length(#data csegs) + 8)
	      (#cn csegs)

	(* This function must be kept in sync with the "write" function below.
	 * It calculates the number of bytes written by a corresponding
	 * call to "write". *)
	fun size { content = bfc, nopickle } = let
	    val BFC { imports, exportPid, senv, cmData, lambda,  ... } = bfc
	    val { unpickled = lut, pickled = lambdaP, ... } = lambda
	    val pidSz = Word8Vector.length (Pid.toBytes (#pid senv))
	    val (_, picki) = pickleImports imports
	    val csegs = codeSegments bfc
	    val hasExports = isSome exportPid
	in
	    magicBytes +
	    9 * 4 +
	    Word8Vector.length picki +
	    (if hasExports then pidSz else 0) +
	    pidSz * (length cmData + 2) +
	    (if nopickle then 0
	     else case lut () of NONE => 0 | _ => Word8Vector.length lambdaP) +
	    codeSize csegs +
	    (if nopickle then 0 else Word8Vector.length (#pickled senv))
	end
	    
	(* Keep this in sync with "size" (see above). *)
	fun write { stream = s, content = bfc, nopickle } = let
	    val BFC { imports, exportPid, cmData, senv, lambda, ... } = bfc
	    val { pickled = senvP, pid = staticPid, ... } = senv
	    val { pickled = lambdaP, pid = lambdaPid, unpickled = lut } =
		lambda
	    val staticPid = staticPid
	    val envPids = staticPid :: lambdaPid :: cmData
	    val (leni, picki) = pickleImports imports
	    val importSzB = Word8Vector.length picki
	    val (ne, epl) =
		case exportPid of
		    NONE => (0, [])
		  | SOME p => (1, [p])
	    val nei = length envPids
	    val cmInfoSzB = nei * bytesPerPid
	    val sa2 =
		(if nopickle then 0
		 else case lut () of
		     NONE => 0 
		   | _ => Word8Vector.length lambdaP)
	    val res1Sz = 0
	    val res2Sz = 0
	    val csegs = codeSegments bfc
	    val cs = codeSize csegs
	    fun codeOut c = (
		  writeInt32 s (CodeObj.size c);
		  CodeObj.output (s, c))
	    val (es, writeEnv) =
		if nopickle then (0, fn () => ())
		else (Word8Vector.length senvP,
		      fn () => BinIO.output (s, senvP))
	in
	    BinIO.output (s, MAGIC);
	    app (writeInt32 s) [leni, ne, importSzB, cmInfoSzB,
				sa2, res1Sz, res2Sz, cs, es];
	    BinIO.output (s, picki);
	    writePidList (s, epl);
	    (* arena1 *)
	    writePidList (s, envPids);
	    (* arena2 -- pickled flint stuff *)
	    if sa2 = 0 then () else BinIO.output (s, lambdaP);
	    (* arena3 is empty *)
	    (* arena4 is empty *)
	    (* code objects *)
	    writeInt32 s (Word8Vector.length (#data csegs));
	    BinIO.output(s, #data csegs);
	    codeOut (#c0 csegs);
	    app codeOut (#cn csegs);
	    writeEnv ()
	end

	fun create args = let
	    val { splitting, cmData, ast, source, corenv, senv, symenv } =
		args
	    val errors = Err.errors source
	    fun check phase =
		if Err.anyErrors errors then raise Compile (phase ^ " failed") 
		else ()
	    val cinfo = C.mkCompInfo (source, corenv, fn x => x)

	    val { csegments=code, newstatenv, exportPid, staticPid, imports,
		  pickle=envPickle, inlineExp, ...} = 
		C.compile { source=source, ast=ast, statenv=senv, 
			    symenv=symenv, compInfo=cinfo, checkErr=check, 
			    splitting=splitting}
	    val {hash = lambdaPid, pickle} = PickMod.pickleFLINT inlineExp
	    fun pd (u, p, x) =
		{ unpickled = fn () => u, pid = p, pickled = x }
	in
	    BFC { imports = imports,
		  exportPid = exportPid,
		  cmData = cmData,
		  senv = pd (newstatenv, staticPid, envPickle),
		  lambda = pd (inlineExp, lambdaPid, pickle),
		  csegments = code,
		  executable = ref NONE }
	end

	fun exec (bfc as BFC { imports, ... }, denv) =
	    C.execute { executable = executableOf bfc,
		        imports = imports,
			exportPid = exportPidOf bfc,
			dynenv = denv }
    end
end
