(* cunitutil.sml
 *
 * Copyright 1989-1995 by AT&T Bell Laboratories.
 *
 * Utility functions for reading and writing of bin files.
 *
 *)

signature CUNITUTIL =
  sig

    exception FormatError
    exception NoCodeBug

    type 'iid cunit
    type pid = PersStamps.persstamp

    type senv = SCEnv.Env.staticEnv
    type symenv = SCEnv.Env.symenv
    type env = SCEnv.Env.environment
    type lambda = Lambda.lexp

    type csegments = {c0: Word8Vector.vector, cn: Word8Vector.vector list, name: string option ref}

    type obj = Unsafe.Object.object

    val readUnit: { name: string,
                    stream: BinIO.instream,
		    pids2iid: pid list -> 'iid,
		    senv: senv,
		    keep_code: bool }
	-> 'iid cunit

    val writeUnit: { stream: BinIO.outstream,
		     cunit: 'iid cunit,
		     keep_code: bool,
		     iid2pids: 'iid -> pid list }
	-> unit
   
    (* the hashing/pickling of environments is already done by the
     * elaborator; lambda's must be hashed/pickled here *)
    val makeUnit: { imports: pid list,
		    exportPid: pid option,
		    references: 'iid,

		    staticPid: pid,
		    newenv: senv,
		    newenvPickle: Word8Vector.vector,

		    lambda_i: lambda option,

		    code: csegments } -> 'iid cunit

    val staticPidCU: 'iid cunit -> pid
    val lambdaPidCU: 'iid cunit -> pid
    val senvCU: 'iid cunit -> senv
    val symenvCU: 'iid cunit -> symenv
    val envCU: 'iid cunit -> env option ref
    val importsCU: 'iid cunit -> pid list
    val exportCU: 'iid cunit -> pid option
    val referencesCU: 'iid cunit -> 'iid

    val nocodeCU: 'iid cunit -> bool
  
    val codeClosure: 'iid cunit -> obj vector -> obj
    val discardCode: 'iid cunit -> unit
  end

functor CUnitUtilFun(
    structure Compile : COMPILE
    structure Machm: CODEGENERATOR
  ) : CUNITUTIL = struct

    structure Pid = PersStamps
    structure Env = SCEnv.Env
    structure Comp = Compile
    structure Err = ErrorMsg

    exception FormatError 
    exception NoCodeBug

    fun error msg = (
	  Control.Print.say(concat["Bin file format error: ", msg, "\n"]);
	  raise FormatError)

    type pid = Pid.persstamp
    type senv = Env.staticEnv
    type symenv = Env.symenv
    type env = Env.environment
    type lambda = Lambda.lexp

    type obj = Unsafe.Object.object
    type objvec = obj Vector.vector

    type csegments = Comp.csegments

    datatype code
      = DISCARDED
      | CSEGS of csegments
      | CLOSURE of objvec -> obj

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

    fun importsCU (CU{ imports, ...}) = imports
    fun exportCU (CU{exportPid, ...}) = exportPid
    fun referencesCU (CU{references, ...}) = references
    fun staticPidCU (CU{ staticPid, ... }) = staticPid
    fun lambdaPidCU (CU{ lambdaPid, ... }) = lambdaPid
    fun senvCU (CU{senv, ... }) = senv
    fun symenvCU (CU{exportPid, lambda, ... }) = 
	Comp.symDelta (exportPid, lambda)
    fun penvCU (CU{penv = pe, ... }) = pe
    fun envCU (CU{env = e, ... }) = e

    fun nocodeCU (CU{ code = ref DISCARDED, ... }) = true
      | nocodeCU _ = false

    fun codeSegments (CU{code = ref (CSEGS s), ... }) = s
      | codeSegments _ = raise NoCodeBug

    fun codeClosure (CU{code, ... }) = (case !code
	   of DISCARDED => raise NoCodeBug
	    | CLOSURE obj => obj
	    | CSEGS s => let
		val obj = Comp.applyCode s
	        in
		  code := CLOSURE obj; obj
	        end
	  (* end case *))

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
	val a = Machm.architecture
	val a = if String.sub (a, 0) = #"."
	      then fit (7, substring (a, 1, (String.size a) - 1))
	      else fit (7, a)
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
		  (bytes, UnpickMod.unpickleLambda{hash=staticPid,pickle=bytes})
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
        val senv = SCStaticEnv.SC b'senv
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
	  val (ne, epl) = (case exportPid of NONE => (0, []) | SOME p => (1, [p]))
	  val nei = length envPids
	  val cmInfoSzB = nei * bytesPerPid
	  val sa2 = (case lambda of NONE => 0 | _ => Word8Vector.length plambda)
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

    fun makeUnit {
	    imports, exportPid, references,
	    staticPid, newenv, newenvPickle,
	    code, lambda_i
	  } = let
	  val {hash = lambdaPid, pickle} = PickMod.pickleLambda lambda_i
	  in
	    CU{
		imports = imports,
        	exportPid = exportPid,
        	references = references,
		staticPid = staticPid,
		senv = newenv,
		penv = newenvPickle,
		lambdaPid = lambdaPid,
        	lambda = lambda_i,
		plambda = pickle,
        	env = ref NONE,
        	code = ref (CSEGS code)
	      }
	  end

  end

(*
 * $Log: cunitutil.sml,v $
 * Revision 1.6  1997/08/25  19:20:03  riccardo
 *   Added support for tagging code objects with their source/bin file name.
 *
 * Revision 1.5  1997/08/11  18:29:39  george
 *   Simplified the modmap handling by no longer paying attention to
 *   space leak problems.  Such problems don't matter in this version,
 *   because modmaps aren't used for the top-level environment.
 * 							-- blume
 *
 * Revision 1.4  1997/07/28  23:04:10  dbm
 *   Fix from Matthias for CM slowdown due to environment building.
 *
 * Revision 1.3  1997/06/30  19:37:03  jhr
 *   Removed System structure; added Unsafe structure.
 *
 * Revision 1.2  1997/02/11  15:16:20  george
 * moved stuff from System to SMLofNJ
 *
 * Revision 1.1.1.1  1997/01/14  01:38:27  george
 *   Version 109.24
 *
 *)
