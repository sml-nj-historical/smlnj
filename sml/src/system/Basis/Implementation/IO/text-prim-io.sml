(* text-prim-io.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure TextPrimIO : TEXT_PRIM_IO = struct

    local
	structure GenericPrimIO =
	GenericPrimIO (structure Vector = CharVector
		       structure Array = CharArray
		       structure VectorSlice = CharVectorSlice
		       structure ArraySlice = CharArraySlice
		       val someElem = #"\000"
		       type pos = Position.int
		       val compare = PositionImp.compare)
    in
        open GenericPrimIO
    end

    val defaultBufferSize = 4096

    fun checkClosed closed =
	if !closed then raise IO.ClosedStream else ()

    fun mkReader { fd = rfd as OS.IO.IODesc fd, name, chunkSize } = let
	val isReg = (SMLBasis.ioDescKind fd = SMLBasis.IOD_KIND_FILE)
	val chunkSize = getOpt (chunkSize, defaultBufferSize)
	val closed = ref false
	fun readVec i = let
	    val _ = checkClosed closed
	    val vopt = SMLBasis.readTextVec (false, fd, i)
	    val v = case vopt of SOME v => v | NONE => CharVector.fromList []
	in
	    v
	end
	fun readVecNB i = let
	    val _ = checkClosed closed
	    val vopt = SMLBasis.readTextVec (true, fd, i)
	in
	    vopt
	end
	fun readArr { buf, i, sz } = let
	    val _ = checkClosed closed
	    val sz = case sz of SOME s => s
			      | NONE => CharArray.length buf
	    val n = SMLBasis.readTextArr (false, fd, buf, i, sz)
	in
	    n
	end
	fun readArrNB { buf, i, sz } = let
	    val _ = checkClosed closed
	    val sz = case sz of SOME s => s
			      | NONE => CharArray.length buf
	    val n = SMLBasis.readTextArr (true, fd, buf, i, sz)
	in
	    if n < 0 then NONE else SOME n
	end
	fun avail () = if !closed then SOME 0 else NONE
	fun close () =
	    if !closed then ()
	    else (closed := true; SMLBasis.closeFile fd)
    in
	RD { name = name,
	     chunkSize = chunkSize,
	     readVec   = SOME readVec,
	     readArr   = SOME readArr,
	     readVecNB = SOME readVecNB,
	     readArrNB = SOME readArrNB,
	     block     = NONE,
	     canInput  = NONE,
	     avail     = avail,
	     getPos    = NONE,
	     setPos    = NONE,
	     endPos    = NONE,
	     verifyPos = NONE,
	     close     = close,
	     ioDesc    = SOME rfd }
    end

    fun mkWriter { fd = rfd as OS.IO.IODesc fd, name, chunkSize } = let
	val isReg = (SMLBasis.ioDescKind fd = SMLBasis.IOD_KIND_FILE)
	val chunkSize = getOpt (chunkSize, defaultBufferSize)
	val closed = ref false
	fun writeVec { buf, i, sz } =
	    let val _ = checkClosed closed
		val sz = case sz of SOME s => s
				  | NONE => CharVector.length buf
	    in
		SMLBasis.writeTextVec (false, fd, buf, i, sz)
	    end
	fun writeArr { buf, i, sz } =
	    let val _ = checkClosed closed
		val sz = case sz of SOME s => s
				  | NONE => CharArray.length buf
	    in
		SMLBasis.writeTextArr (false, fd, buf, i, sz)
	    end
	fun writeVecNB { buf, i, sz } =
	    let val _ = checkClosed closed
		val sz = case sz of SOME s => s
				  | NONE => CharVector.length buf
		val n = SMLBasis.writeTextVec (true, fd, buf, i, sz)
	    in
		if n < 0 then NONE else SOME n
	    end
	fun writeArrNB { buf, i, sz } =
	    let val _ = checkClosed closed
		val sz = case sz of SOME s => s
				  | NONE => CharArray.length buf
		val n = SMLBasis.writeTextArr (true, fd, buf, i, sz)
	    in
		if n < 0 then NONE else SOME n
	    end
	fun close () =
	    if !closed then ()
	    else (closed := true; SMLBasis.closeFile fd)
    in
	WR { name = name,
	     chunkSize  = chunkSize,
	     writeVec   = SOME writeVec,
	     writeArr   = SOME writeArr,
	     writeVecNB = SOME writeVecNB,
	     writeArrNB = SOME writeArrNB,
	     block      = NONE,
	     canOutput  = NONE,
	     getPos     = NONE,
	     setPos     = NONE,
	     endPos     = NONE,
	     verifyPos  = NONE,
	     ioDesc     = SOME rfd,
	     close      = close }
    end

    local
	open SMLBasis
	(* are these right ? *)
	val openRdFlags = OPEN_RD
	val openWrFlags = OPEN_WR + OPEN_CREATE + OPEN_TRUNC
	val openAppFlags = OPEN_WR + OPEN_CREATE + OPEN_APPEND
	val D = OS.IO.IODesc
    in
        fun openRd fname =
	    mkReader { fd = D (openFile (fname, openRdFlags)),
		       name = fname,
		       chunkSize = NONE }

	fun openWr fname =
	    mkWriter { fd = D (openFile (fname, openWrFlags)),
		       name = fname,
		       chunkSize = NONE }

	fun openApp fname =
	    mkWriter { fd = D (openFile (fname, openAppFlags)),
		       name = fname,
		       chunkSize = NONE }
	fun stdIn () =
	    mkReader { fd = D (SMLBasis.getStdIn ()),
		       name = "<stdin>",
		       chunkSize = NONE }
	fun stdOut () =
	    mkWriter { fd = D (SMLBasis.getStdOut ()),
		       name = "<stdout>",
		       chunkSize = NONE }
	fun stdErr () =
	    mkWriter { fd = D (SMLBasis.getStdErr ()),
		       name = "<stderr>",
		       chunkSize = NONE }
    end

    val strReader = openVector
end
