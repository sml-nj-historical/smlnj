(* COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies *)
(* batchutil.sml *)

functor BatchUtilFun(C : COMPILE) : BATCHUTIL =
struct
    structure Pid = PersStamps
    structure Env = CMEnv.Env
    structure Err = ErrorMsg
    structure CB = CompBasic

    exception FormatError 
    exception NoCodeBug

    fun error msg = (
      Control.Print.say(concat["Bin file format error: ", msg, "\n"]);
      raise FormatError)

(***************************************************************************
 *                UTILITY FUNCTIONS FOR MANIPULATING CUNIT                 *
 ***************************************************************************)

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

    datatype 'iid cunit = CU of {
	imports: C.import list,
	exportPid: pid option,
	references: 'iid,
	staticPid: pid,
	senv: senv,
	penv: Word8Vector.vector,
	lambdaPid: pid,
	lambda: lambda option,
	plambda: Word8Vector.vector,
	env: env option ref,
	code: code ref
      }

    val arch = C.architecture
    fun importsCU (CU{ imports, ...}) = imports
    fun exportCU (CU{exportPid, ...}) = exportPid
    fun referencesCU (CU{references, ...}) = references
    fun staticPidCU (CU{ staticPid, ... }) = staticPid
    fun lambdaPidCU (CU{ lambdaPid, ... }) = lambdaPid
    fun senvCU (CU{senv, ... }) = senv
    fun symenvCU (CU{exportPid, lambda, ... }) = C.mksymenv (exportPid, lambda)
    fun penvCU (CU{penv = pe, ... }) = pe
    fun envCU (CU{env = e, ... }) = e

    fun nocodeCU (CU{ code = ref DISCARDED, ... }) = true
      | nocodeCU _ = false

    fun codeSegments (CU{code = ref (CSEGS s), ... }) = s
      | codeSegments _ = raise NoCodeBug

    fun discardCode (CU{code, ... }) = code := DISCARDED

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
 *         24  4  size of CM-specific info in bytes (cmInfoSzB)
 *         28  4  size of pickled lambda-expression in bytes (lambdaSzB)
 *         32  4  size of reserved area 1 in bytes (reserved1)
 *         36  4  size of reserved area 2 in bytes (reserved2)
 *         40  4  size of code area in bytes (codeSzB)
 *         44  4  size of pickled environment in bytes (envSzB)
 *         48  i  import trees [This area contains pickled import trees --
 *                  see below.  The total number of leaves in these trees is
 *                  importCnt.  The size impSzB of this area depends on the
 *                  shape of the trees.]
 *       i+48 ex  export pids [Each export pid occupies 16 bytes. Thus, the
 *                  size ex of this area is 16*exportCnt (0 or 16).]
 *    ex+i+48 cm  CM info [Currently a list of pid-pairs.] (cm = cmInfoSzB)
 * ----END OF HEADER----
 *          0  h  HEADER (h = 48+cm+ex+i)
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
 *        recur_write_ul (l / 0200, file);
 *        putc ((l % 0200) | 0200, file);
 *      }
 *    }
 *
 *    void write_ul (unsigned long l, FILE *file)
 *    {
 *      recur_write_ul (l / 0200, file);
 *      putc (l % 0200, file);
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
	val a = if String.sub (arch, 0) = #"."
	      then fit (7, substring (arch, 1, (String.size arch) - 1))
	      else fit (7, arch)
	in
	  Byte.stringToBytes (concat [v, a, "\n"])
	end

    val magicBytes = Word8Vector.length MAGIC
    val bytesPerPid = 16

    fun bytesIn (s, n) = let
	  val bv = BinIO.inputN (s, n)
	  in
	    if (n = Word8Vector.length bv)
	      then bv
	      else error(concat[
		  "expected ", Int.toString n, " byte, but found ",
		  Int.toString(Word8Vector.length bv)
	        ])
	  end

    fun readInt32 s = LargeWord.toIntX(Pack32Big.subVec(bytesIn(s, 4), 0))

    fun readPackedInt32 s = let
	fun loop n =
	    case BinIO.input1 s of
		NONE => error "unable to read a packed int32"
	      | SOME w8 => let
		    val n' =
			n * 0w128 + Word8.toLargeWord (Word8.andb (w8, 0w127))
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
	  val cmInfoSzB = readInt32 s
	  val nei = cmInfoSzB div bytesPerPid
	  val sLam = readInt32 s
	  val sa1 = readInt32 s
	  val sa2 = readInt32 s
	  val cs = readInt32 s
	  val es = readInt32 s
	  val imports = readImports (s, leni)
	  val exportPid = (case ne
	         of 0 => NONE
		  | 1 => SOME(readPid s)
		  | _ => error "too many export PIDs"
		(* end case *))
	  val envPids = readPidList (s, nei)
	  in
	    case envPids
	     of (st :: lm :: references) => {
		  nExports = ne,
		  lambdaSz = sLam,
		  res1Sz = sa1, res2Sz = sa2, codeSz = cs, envSz = es,
		  imports = imports, exportPid = exportPid,
		  references = references, staticPid = st, lambdaPid = lm
		}
	      | _ => error "env PID list"
	    (* end case *)
	  end

    (* must be called with second arg >= 0 *)
    fun readCodeList (_, 0) = []
      | readCodeList (s, n) = let
	  val sz = readInt32 s
	  val n' = n - sz - 4
	  val c = if n' < 0 then error "code size" else bytesIn (s, sz)
	  in
	    c :: readCodeList (s, n')
	  end

    fun readUnit {name=n, stream = s, pids2iid, senv = context, keep_code} = let
        val { nExports = ne, lambdaSz = sa2,
	      res1Sz, res2Sz, codeSz = cs, envSz = es,
	      imports, exportPid, references,
	      staticPid, lambdaPid
	    } = readHeader s
	val iid = pids2iid references
	val (plambda, lambda_i) = if sa2 = 0
	      then (Word8Vector.fromList [], NONE)
	      else let
		val bytes = bytesIn (s, sa2)
		in
		  (bytes, 
                   UnpickMod.unpickleFLINT{hash=staticPid,pickle=bytes})
		end
	val _ = if res1Sz = 0 andalso res2Sz = 0
	      then () else error "non-zero reserved size"
	val code = (case readCodeList (s, cs) of
			data :: c0 :: cn => { data = data, c0 = c0, cn = cn,
					      name = ref (SOME n) }
		      | _ => error "missing code objects")
	val penv = bytesIn (s, es)
	val _ = if Word8Vector.length penv = es andalso BinIO.endOfStream s
		then ()
		else error "missing/excess bytes in bin file"
	val b'senv = UnpickMod.unpickleEnv (context, { hash = staticPid,
						       pickle = penv })
        val senv = CMStaticEnv.CM b'senv
        in
	    CU {
	      imports = imports, exportPid = exportPid, references = iid,
	      staticPid = staticPid, senv = senv, penv = penv,
	      lambdaPid = lambdaPid, lambda = lambda_i, plambda = plambda,
	      env = ref NONE,
	      code = ref (if keep_code then CSEGS code else DISCARDED)
	    }
        end

    fun writeUnit {stream = s, cunit = u, keep_code, iid2pids} = let
	val CU { imports, exportPid, references,
		 staticPid, penv,
		 lambdaPid, lambda, plambda, ...
	       } = u
	val envPids = staticPid :: lambdaPid :: iid2pids references
	val (leni, picki) = pickleImports imports
	val (ne, epl) =
	    (case exportPid of
		 NONE => (0, []) 
	       | SOME p => (1, [p]))
	val nei = length envPids
	val cmInfoSzB = nei * bytesPerPid
	val sa2 =
	    (case lambda of
		 NONE => 0 
	       | _ => Word8Vector.length plambda)
	val res1Sz = 0
	val res2Sz = 0
	val { data, c0, cn , ...} = codeSegments u
	fun csize c = (Word8Vector.length c) + 4 (* including size field *)
	val cs = foldl (fn (c, a) => (csize c) + a) (csize c0 +csize data) cn
	fun codeOut c = (writeInt32 s (Word8Vector.length c);
			 BinIO.output (s, c))
    in
	BinIO.output (s, MAGIC);
	app (writeInt32 s) [leni, ne, cmInfoSzB];
	app (writeInt32 s) [sa2, res1Sz, res2Sz, cs];
	writeInt32 s (Word8Vector.length penv);
	BinIO.output (s, picki);
	writePidList (s, epl);
	(* arena1 *)
	writePidList (s, envPids);
	(* arena2 *)
	case lambda of
	    NONE => ()
	  | _ => BinIO.output (s, plambda);
	(* arena3 is empty *)
	(* arena4 is empty *)
	(* code objects *)
	codeOut data;
	codeOut c0;
	app codeOut cn;
	BinIO.output (s, penv);
	if keep_code then () else discardCode u
    end


(***************************************************************************
 *                UTILITY FUNCTIONS THAT SUPPORTS BATCH COMPILATION        *
 ***************************************************************************)
    exception Compile = C.Compile
    exception TopLevelException = C.TopLevelException
    exception SilentException = C.SilentException

    val parse = C.parse
    val makePid = C.makePid

    fun makeUnit {runtimePid : pid option, splitting, references,
                  ast, source, corenv, senv : senv, symenv : symenv} = let
	val errors = Err.errors source
        fun check phase =
  	  if Err.anyErrors errors then raise Compile (phase ^ " failed") 
          else ()
	val cinfo = C.mkCompInfo (source, corenv, fn x => x)

        val {csegments=code, newstatenv, exportPid, staticPid, imports,
             pickle=envPickle, inlineExp, ...} = 
               C.compile {source=source, ast=ast, statenv=senv, 
                          symenv=symenv, compInfo=cinfo, checkErr=check, 
                          runtimePid=runtimePid, splitting=splitting}
        val {hash = lambdaPid, pickle} = PickMod.pickleFLINT inlineExp
        in
	    CU{
		imports = imports,
        	exportPid = exportPid,
        	references = references,
		staticPid = staticPid,
		senv = newstatenv,
		penv = envPickle,
		lambdaPid = lambdaPid,
        	lambda = inlineExp,
		plambda = pickle,
        	env = ref NONE,
        	code = ref (CSEGS code)
	      }
        end (* function makeUnit *)

    fun codeClosure (CU{code, ... }) = (case !code
	   of DISCARDED => raise NoCodeBug
	    | CLOSURE obj => obj
	    | CSEGS s => let
		val obj = C.mkexec s
	        in
		  code := CLOSURE obj; obj
	        end
	  (* end case *))

    fun execUnit (u, denv) = let
        val ndenv = 
            C.execute { executable = C.isolate(codeClosure u),
                        imports = importsCU u,
		        exportPid = exportCU u,
		        dynenv = denv }
        in 
            Env.mkenv {static = senvCU u,
                       dynamic = ndenv,
                       symbolic = symenvCU u}
        end

end (* functor BatchUtilFun *)

(*
 * $Log: batchutil.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:15  george
 * Version 110.5
 *
 *)
