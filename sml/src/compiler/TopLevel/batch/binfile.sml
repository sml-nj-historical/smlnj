(* COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies *)
(* binfile.sml *)

functor BinfileFun (C : COMPILE) : BINFILE = struct
    structure Pid = PersStamps
    structure Env = CMEnv.Env
    structure Err = ErrorMsg
    structure CB = CompBasic

    exception FormatError = CodeObj.FormatError
    exception NoCodeBug
    exception NoPickleBug

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

    datatype code
      = DISCARDED
      | CSEGS of csegments
      | CLOSURE of executable

    (* pickled data: the data, the pid, and the pickle *)
    type 'a pData =
       { unpickled: 'a,
	 pid: pid,
	 pickled: Word8Vector.vector option ref }

    fun dropPickle { pickled, pid, unpickled } = pickled := NONE

    datatype bfContent =
	BFC of {
		imports: C.import list,
		hasExports: bool,
		cmData: pid list,
		senv: senv pData,
		lambda: lambda option pData,
		code: code ref
	       }

    fun staticPidOf (BFC { senv = { pid, ... }, ... }) = pid
    fun exportPidOf (bfc as BFC { hasExports, ... }) =
	if hasExports then SOME (staticPidOf bfc) else NONE
    fun lambdaPidOf (BFC { lambda = { pid, ... }, ... }) = pid
    fun cmDataOf (BFC { cmData, ... }) = cmData
    fun senvOf (BFC { senv = { unpickled, ... }, ... }) = unpickled
    fun symenvOf (bfc as BFC { senv, lambda, hasExports, ... }) =
	C.mksymenv (exportPidOf bfc, #unpickled lambda)

    fun discardCode (BFC { code, ... }) = code := DISCARDED
    fun noCode (BFC { code = ref DISCARDED, ... }) = true
      | noCode _ = false

    local
	val arch = C.architecture

	fun executableOf (BFC { code = ref (CLOSURE e), ... }) = e
	  | executableOf (BFC { code = ref DISCARDED, ... }) = raise NoCodeBug
	  | executableOf (BFC { code = cref as ref (CSEGS cs), ... }) = let
		val e = C.isolate (C.mkexec cs)
	    in
		cref := CLOSURE e; e
	    end

	fun codeSegments (BFC {code = ref (CSEGS s), ... }) = s
	  | codeSegments _ = raise NoCodeBug

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
	    if (bytesIn (s, magicBytes)) = MAGIC then ()
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
	    val { name, stream = s, senv = context, keep_code} = args
	    val { nExports = ne, lambdaSz = sa2,
		  res1Sz, res2Sz, codeSz = cs, envSz = es,
		  imports, exportPid, cmData,
		  staticPid, lambdaPid } = readHeader s
	    val lambda_i =
		if sa2 = 0 then NONE
		else let
		    val bytes = bytesIn (s, sa2)
		in
		    UnpickMod.unpickleFLINT { hash = staticPid, pickle = bytes}
		end
	    val _ = if res1Sz = 0 andalso res2Sz = 0
			then () else error "non-zero reserved size"
	    val code = readCodeList (s, name, cs)
	    val penv = bytesIn (s, es)
	    val _ =
		if Word8Vector.length penv = es then ()
		else error "missing bytes in bin file"
	    val b'senv = UnpickMod.unpickleEnv (context, { hash = staticPid,
							   pickle = penv })
	    val senv = CMStaticEnv.CM b'senv
	    val hasExports = isSome exportPid
	    fun pd (u, p) = { unpickled = u, pid = p, pickled = ref NONE }
        in
	    BFC { imports = imports,
		  hasExports = hasExports,
		  cmData = cmData,
		  senv = pd (senv, staticPid),
		  lambda = pd (lambda_i, lambdaPid),
		  code = ref (if keep_code then CSEGS code else DISCARDED) }
        end

	fun write {stream = s, content = bfc, keep_code } = let
	    val BFC { imports, hasExports, cmData, senv, lambda, ... } = bfc
	    val { pickled = ref senvP, pid = staticPid, ... } = senv
	    val { pickled = ref lambdaP, pid = lambdaPid, unpickled = lu } =
		lambda
	    val senvP = valOf senvP handle Option => raise NoPickleBug
	    val lambdaP = valOf lambdaP handle Option => raise NoPickleBug
	    val staticPid = staticPid
	    val envPids = staticPid :: lambdaPid :: cmData
	    val (leni, picki) = pickleImports imports
	    val importSzB = Word8Vector.length picki
	    val (ne, epl) = if hasExports then (1, [staticPid]) else (0, [])
	    val nei = length envPids
	    val cmInfoSzB = nei * bytesPerPid
	    val sa2 =
		(case lu of
		     NONE => 0 
		   | _ => Word8Vector.length lambdaP)
	    val res1Sz = 0
	    val res2Sz = 0
	    val csegs = codeSegments bfc
	  (* compute size of code objects (including length fields) *)
	    val cs = List.foldl
		  (fn (co, n) => n + CodeObj.size co + 4)
		    (CodeObj.size(#c0 csegs) + Word8Vector.length(#data csegs) + 8)
		      (#cn csegs)
	    fun codeOut c = (
		  writeInt32 s (CodeObj.size c);
		  CodeObj.output (s, c))
	in
	    BinIO.output (s, MAGIC);
	    app (writeInt32 s) [leni, ne, importSzB, cmInfoSzB];
	    app (writeInt32 s) [sa2, res1Sz, res2Sz, cs];
	    writeInt32 s (Word8Vector.length senvP);
	    BinIO.output (s, picki);
	    writePidList (s, epl);
	    (* arena1 *)
	    writePidList (s, envPids);
	    (* arena2 *)
	    case lu of NONE => () | _ => BinIO.output (s, lambdaP);
	    (* arena3 is empty *)
	    (* arena4 is empty *)
	    (* code objects *)
	    writeInt32 s (Word8Vector.length(#data csegs));
	      BinIO.output(s, #data csegs);
	    codeOut (#c0 csegs);
	    app codeOut (#cn csegs);
	    BinIO.output (s, senvP);
	    if keep_code then () else discardCode bfc;
	    dropPickle lambda;
	    dropPickle senv
	end

	fun create args = let
	    val { runtimePid, splitting, cmData,
		  ast, source, corenv, senv, symenv } = args
	    val errors = Err.errors source
	    fun check phase =
		if Err.anyErrors errors then raise Compile (phase ^ " failed") 
		else ()
	    val cinfo = C.mkCompInfo (source, corenv, fn x => x)

	    val { csegments=code, newstatenv, exportPid, staticPid, imports,
		  pickle=envPickle, inlineExp, ...} = 
		C.compile { source=source, ast=ast, statenv=senv, 
			    symenv=symenv, compInfo=cinfo, checkErr=check, 
			    runtimePid=runtimePid, splitting=splitting}
	    val {hash = lambdaPid, pickle} = PickMod.pickleFLINT inlineExp
	    val hasExports = isSome exportPid
	    fun pd (u, p, x) =
		{ unpickled = u, pid = p, pickled = ref (SOME x) }
	in
	    BFC { imports = imports,
		  hasExports = hasExports,
		  cmData = cmData,
		  senv = pd (newstatenv, staticPid, envPickle),
		  lambda = pd (inlineExp, lambdaPid, pickle),
		  code = ref (CSEGS code) }
	end

	fun exec (bfc as BFC { imports, ... }, denv) = let
	    val ndenv = 
		C.execute { executable = executableOf bfc,
			    imports = imports,
			    exportPid = exportPidOf bfc,
			    dynenv = denv }
	in
	    Env.mkenv { static = senvOf bfc,
		        dynamic = ndenv,
			symbolic = symenvOf bfc }
	end
    end
end

