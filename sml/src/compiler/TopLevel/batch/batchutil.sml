(* COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies *)
(* batchutil.sml *)

functor BatchUtilFun(C : COMPILE) : BATCHUTIL =
struct
    structure Pid = PersStamps
    structure Env = CMEnv.Env
    structure Err = ErrorMsg

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
    type lambda = CompBasic.flint

    type csegments = C.csegments
    type executable = C.executable

    datatype code
      = DISCARDED
      | CSEGS of csegments
      | CLOSURE of executable

    datatype 'iid cunit = CU of {
	imports: pid list,
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
   * layout of binfiles:
   *  - 0..x-1:			magic string (length = x)
   *  - x..x+3:			# of imports (= y)
   *  - x+4..x+7:		# of exports (= z)
   *  - x+8..x+11:		size CM-info = (# of envPids) * bytesPerPid
   *  - x+12..x+15:		size lambda_i
   *  - x+16..x+19:		size reserved area1
   *  - x+20..x+23:		size reserved area2
   *  - x+24..x+27:		size code
   *  - x+28..x+31:		size env
   *  - x+32..x+y+31:		import pids
   *  - x+y+32..x+y+z+31:	export pids
   *  - ...			CM-specific info (env pids)
   *  - 			lambda_i
   *  - 			reserved area1
   *  - 			reserved area2
   *  -				code
   *  -				pickled_env
   *  EOF
   *
   * All counts and sizes are represented in big-endian layout.
   * This should be tracked by the run-time system header file
   * "runtime/include/bin-file.h"
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

    fun readPid s = Pid.fromBytes (bytesIn (s, bytesPerPid))
    fun readPidList (s, n) = List.tabulate (n, fn _ => readPid s)

    fun writeInt32 s i =  let
	  val w = fromInt i
	  fun out w = BinIO.output1 (s, toByte w)
	  in
	    out (w >> 0w24); out (w >> 0w16);  out (w >> 0w8); out w
	  end

    fun writePid (s, pid) = BinIO.output (s, Pid.toBytes pid)
    fun writePidList (s, l) = app (fn p => writePid (s, p)) l

    fun checkMagic s =
	  if (bytesIn (s, magicBytes)) = MAGIC then () else error "bad magic number"

    fun readHeader s = let
	  val _ = checkMagic s
	  val ni = readInt32 s
	  val ne = readInt32 s
	  val cmInfoSzB = readInt32 s
	  val nei = cmInfoSzB div bytesPerPid
	  val sLam = readInt32 s
	  val sa1 = readInt32 s
	  val sa2 = readInt32 s
	  val cs = readInt32 s
	  val es = readInt32 s
	  val imports = readPidList (s, ni)
	  val exportPid = (case ne
	         of 0 => NONE
		  | 1 => SOME(readPid s)
		  | _ => error "too many export PIDs"
		(* end case *))
	  val envPids = readPidList (s, nei)
	  in
	    case envPids
	     of (st :: lm :: references) => {
		  nImports = ni, nExports = ne,
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
        val { nImports = ni, nExports = ne, lambdaSz = sa2,
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
	val code = (case readCodeList (s, cs)
	       of [] => error "missing code objects"
		| c0 :: cn => { c0 = c0, cn = cn, name=ref(SOME(n)) }
	      (* end case *))
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
	  val CU{
		  imports, exportPid, references,
		  staticPid, penv,
		  lambdaPid, lambda, plambda, ...
		} = u
	  val envPids = staticPid :: lambdaPid :: iid2pids references
	  val ni = length imports
	  val (ne, epl) = (case exportPid of NONE => (0, []) 
                                           | SOME p => (1, [p]))
	  val nei = length envPids
	  val cmInfoSzB = nei * bytesPerPid
	  val sa2 = (case lambda of NONE => 0 
                                  | _ => Word8Vector.length plambda)
	  val res1Sz = 0
	  val res2Sz = 0
	  val { c0, cn , ...} = codeSegments u
	  fun csize c = (Word8Vector.length c) + 4 (* including size field *)
	  val cs = foldl (fn (c, a) => (csize c) + a) (csize c0) cn
	  fun codeOut c = (
		writeInt32 s (Word8Vector.length c);
		BinIO.output (s, c))
        in
	  BinIO.output (s, MAGIC);
	  app (writeInt32 s) [ni, ne, cmInfoSzB];
	  app (writeInt32 s) [sa2, res1Sz, res2Sz, cs];
	  writeInt32 s (Word8Vector.length penv);
	  writePidList (s, imports);
	  writePidList (s, epl);
	(* arena1 *)
	  writePidList (s, envPids);
	(* arena2 *)
	  case lambda of NONE => () | _ => BinIO.output (s, plambda);
	(* arena3 is empty *)
	(* arena4 is empty *)
	(* code objects *)
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
            C.execute { executable = codeClosure u,
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
