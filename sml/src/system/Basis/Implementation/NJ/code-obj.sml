(* code-obj.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * An interface to manipulating code objects.
 *
 *)

structure CodeObj :> sig

    type code_obj
    type csegments = {c0 : code_obj, cn : code_obj list}

    val size : code_obj -> int
    val name : code_obj -> string option

    val alloc : (int * string option) -> code_obj
	(* allocate an uninitialized code object of the given size and
	 * name.  The size of the resulting code object will be the given
	 * size plus the length of the name.
	 *)

    val reify : csegments -> (object vector -> object)
	(* turn a code object into an executable ML function *)

  (* read/write code objects from/to a binary stream. *)
    val read  : (BinIO.instream * int) -> code_obj
    val write : (BinIO.outstream * code_obj) -> unit

  (* operations for initializing code objects.  These should not be called
   * after reification.
   *)
    val update : (code_obj * int * Word8.word) -> unit

  end = struct

    structure W8A = Word8Array
    structure W8V = Word8Vector
    structure BIO = BinIO.StreamIO

    val unsafeUpdate = InlineT.Word8Array.update

  (* When the new array/vector representation gets implemented, we can
   * represent this as a Word8Array and use the Word8Array operations
   * directly.
   *)
    datatype code_obj = CObj of {
	len : int,
	data : W8A.array
      }

    type csegments = {c0 : code_obj, cn : code_obj list}

  (* various run-time system operations *)
    val getName    : W8A.array -> string option
	  CInterface.c_function "SMLNJ-RunT" "getCodeName"
    val allocCode  : (int * string option) -> W8A.array
	  CInterface.c_function "SMLNJ-RunT" "allocCode"
    val flushCache : W8A.array -> unit
	  CInterface.c_function "SMLNJ-RunT" "flushCache"

    fun size (CObj{len, ...}) = len
    fun name (CObj{data, ...}) = getName data

  (* allocate an uninitialized code object of the given size and
   * name.  The size of the resulting code object will be the given
   * size plus the length of the name.
   *)
    fun alloc (codeSz, name) = let
	  val trueSz = (case name
		 of NONE => codeSz
		  | (SOME s) => codeSz + String.size s
		(* end case *))
	  in
	    CObj{len = trueSz, data = allocCode (codeSz, name)}
	  end

  (* turn a code object into an executable ML function *)
    fun reify {c0, cn} = let
	  val mkCodeV : code_obj -> (object vector -> object) =
		InlineT.cast flushCache
	  val mkCodeO : code_obj -> (object -> object) =
		InlineT.cast flushCache
	  fun mk ([], f) = f
	    | mk (c::r, f) = let val g = mkCodeO c
		in
		  mk (r, fn obj => g(f obj))
		end
	  in
	    mk (cn, mkCodeV c0)
	  end

  (* read/write code objects from/to a binary stream. *)
    fun read (inStrm, sz) = let
	  val (rd as BinPrimIO.RD{readArr=SOME readArr, ...}, buf) =
		BIO.getReader(BinIO.getInstream inStrm)
	  val bufSz = W8V.length buf
	  val codeObj = allocCode (codeSz, NONE)
	  fun readCode (i, n) = readArr {buf=codeObj, i=i, sz=SOME n}
	  fun copyCode len = W8A.copyVec{
		  di=0, dst=codeObj, si=0, src=buf, len=len
		}
	  val buf = if (bufSz = 0)
		  then (readCode (0, 0); NONE)
		else if (bufSz < sz)
		  then (copyCode NONE; readCode (bufSz, sz-bufSz); NONE)
		else if (bufSz = sz)
		  then (copyCode NONE; NONE)
		  else (copyCode(SOME sz); SOME(W8V.extract(buf, sz, NONE)))
	  in
	    BinIO.setInstream (inStrm, BIO.mkInstream (rd, buf));
	    CObj{len = sz, data = codeObj}
	  end

    fun write (outStrm, CObj{len, data}) = let
	  val (wr as BinPrimIO.WR{writeArr=SOME writeArr, ...}, mode) =
		BIO.getWriter(BinIO.getOutstream outStrm)
	  in
	    writeArr {buf=data, i=0, sz=NONE};
	    BIO.setOutstream (outStrm, BIO.mkOutstream (wr, mode))
	  end

    fun check (len, i) =
	  if (InlineT.DfltInt.ltu(len, i)) then raise General.Subscript else ()

    fun update (CObj{len, data}, i, w) = (check(); unsafeUpdate(data, i, w))

  end;


