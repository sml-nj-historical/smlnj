(* bin-prim-io.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 2001 Lucent Technologies, Bell Labs
 *
 *)
local
    structure Position = PositionImp
    structure Int32 = Int32Imp
in
structure BinPrimIO : BIN_PRIM_IO = struct

    local
	structure GenericPrimIO =
	GenericPrimIO (structure Vector = Word8Vector
		       structure Array = Word8Array
		       structure VectorSlice = Word8VectorSlice
		       structure ArraySlice = Word8ArraySlice
		       val someElem = (0w0 : Word8.word)
		       type pos = Position.int
		       val compare = Position.compare)
    in
        open GenericPrimIO
    end

    val defaultBufferSize = 4096

    fun checkClosed closed =
	if !closed then raise IO.ClosedStream else ()

    val p2i = Int32.fromLarge o Position.toLarge
    val i2p = Position.fromLarge o Int32.toLarge

    fun fileSizeIODesc fd = let
	val cur = SMLBasis.getPos fd
    in
	SMLBasis.setPos (fd, 0, SMLBasis.SET_POS_END);
	SMLBasis.getPos fd
	before SMLBasis.setPos (fd, cur, SMLBasis.SET_POS_BEGIN)
    end
	

    fun iodEndPos fd = i2p (fileSizeIODesc fd)
    fun iodCurPos fd = i2p (SMLBasis.getPos fd)

    fun posFns (closed, true, fd) =
	let val pos = ref (0 : Position.int)
	    fun getPos () = !pos
	    fun setPos p =
		(checkClosed closed;
		 SMLBasis.setPos (fd, p2i p, SMLBasis.SET_POS_CUR);
		 pos := p + !pos)
	    fun endPos () = (checkClosed closed;
			     iodEndPos fd)
	    fun verifyPos () = let
		val curPos = iodCurPos fd
	    in
		pos := curPos; curPos
	    end
	in
	    { pos = pos, getPos = SOME getPos, setPos = SOME setPos,
	      endPos = SOME endPos, verifyPos = SOME verifyPos }
	end
      | posFns (closed, false, fd) =
	{ pos = ref (0 : Position.int),
	  getPos = NONE, setPos = NONE, endPos = NONE, verifyPos = NONE }

    fun mkReader { fd = rfd as ref fd, name, chunkSize } = let
	val isReg = (SMLBasis.ioDescKind fd = SMLBasis.IOD_KIND_FILE)
	val chunkSize = getOpt (chunkSize, defaultBufferSize)
	val closed = ref false
	val { pos, getPos, setPos, endPos, verifyPos } =
	    posFns (closed, isReg, fd)
	fun incPos k = pos := k + !pos
	fun readVec i = let
	    val _ = checkClosed closed
	    val vopt = SMLBasis.readBinVec (false, fd, i)
	    val v = case vopt of SOME v => v | NONE => Word8Vector.fromList []
	    val n = Word8Vector.length v
	in
	    incPos (Position.fromInt n);
	    v
	end
	fun readVecNB i = let
	    val _ = checkClosed closed
	    val vopt = SMLBasis.readBinVec (true, fd, i)
	    val n = case vopt of SOME v => Word8Vector.length v | NONE => 0
	in
	    incPos (Position.fromInt n);
	    vopt
	end
	fun readArr { buf, i, sz } = let
	    val _ = checkClosed closed
	    val sz = case sz of SOME s => s
			      | NONE => Word8Array.length buf
	    val n = SMLBasis.readBinArr (false, fd, buf, i, sz)
	in
	    incPos (Position.fromInt n);
	    n
	end
	fun readArrNB { buf, i, sz } = let
	    val _ = checkClosed closed
	    val sz = case sz of SOME s => s
			      | NONE => Word8Array.length buf
	    val n = SMLBasis.readBinArr (true, fd, buf, i, sz)
	in
	    if n < 0 then NONE
	    else (incPos (Position.fromInt n); SOME n)
	end
	fun avail () =		(* original behavior (should we change it?) *)
	    if !closed then SOME 0
	    else if isReg then SOME (iodEndPos fd - !pos)
	    else NONE
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
	     getPos    = getPos,
	     setPos    = setPos,
	     endPos    = endPos,
	     verifyPos = verifyPos,
	     close     = close,
	     ioDesc    = SOME rfd }
    end

    fun mkWriter { fd = rfd as ref fd, name, chunkSize } = let
	val isReg = (SMLBasis.ioDescKind fd = SMLBasis.IOD_KIND_FILE)
	val chunkSize = getOpt (chunkSize, defaultBufferSize)
	val closed = ref false
	val { pos, getPos, setPos, endPos, verifyPos } =
	    posFns (closed, isReg, fd)
	fun incPos k = pos := k + !pos
	fun writeVec { buf, i, sz } =
	    let val _ = checkClosed closed
		val sz = case sz of SOME s => s
				  | NONE => Word8Vector.length buf
	    in
		SMLBasis.writeBinVec (false, fd, buf, i, sz)
	    end
	fun writeArr { buf, i, sz } =
	    let val _ = checkClosed closed
		val sz = case sz of SOME s => s
				  | NONE => Word8Array.length buf
	    in
		SMLBasis.writeBinArr (false, fd, buf, i, sz)
	    end
	fun writeVecNB { buf, i, sz } =
	    let val _ = checkClosed closed
		val sz = case sz of SOME s => s
				  | NONE => Word8Vector.length buf
		val n = SMLBasis.writeBinVec (true, fd, buf, i, sz)
	    in
		if n < 0 then NONE else SOME n
	    end
	fun writeArrNB { buf, i, sz } =
	    let val _ = checkClosed closed
		val sz = case sz of SOME s => s
				  | NONE => Word8Array.length buf
		val n = SMLBasis.writeBinArr (true, fd, buf, i, sz)
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
	     getPos     = getPos,
	     setPos     = setPos,
	     endPos     = endPos,
	     verifyPos  = verifyPos,
	     ioDesc     = SOME rfd,
	     close      = close }
    end

    local
	open SMLBasis
	(* are these right ? *)
	val openRdFlags = OPEN_RD
	val openWrFlags = OPEN_WR + OPEN_CREATE + OPEN_TRUNC
	val openAppFlags = OPEN_WR + OPEN_CREATE + OPEN_APPEND
    in
        fun openRd fname =
	    mkReader { fd = ref (openFile (fname, openRdFlags)),
		       name = fname,
		       chunkSize = NONE }

	fun openWr fname =
	    mkWriter { fd = ref (openFile (fname, openWrFlags)),
		       name = fname,
		       chunkSize = NONE }

	fun openApp fname =
	    mkWriter { fd = ref (openFile (fname, openAppFlags)),
		       name = fname,
		       chunkSize = NONE }
    end
end
end
