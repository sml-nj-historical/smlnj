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

    fun mkReader { fd, name, chunkSize } =
	raise Fail "mkReader not yet implemented"

    fun mkWriter { fd, name, chunkSize } =
	raise Fail "mkWriter not yet implemented"

    local
	open SMLBasis
	(* are these right ? *)
	val openRdFlags = OPEN_RD
	val openWrFlags = OPEN_WR + OPEN_CREATE + OPEN_TRUNC
	val openAppFlags = OPEN_WR + OPEN_CREATE + OPEN_APPEND
    in
        fun openRd fname =
	    mkReader { fd = openFile (fname, openRdFlags),
		       name = fname,
		       chunkSize = NONE }

	fun openWr fname =
	    mkWriter { fd = openFile (fname, openWrFlags),
		       name = fname,
		       chunkSize = NONE }

	fun openApp fname =
	    mkWriter { fd = openFile (fname, openAppFlags),
		       name = fname,
		       chunkSize = NONE }
    end

    fun stdIn () =
	mkReader { fd = SMLBasis.getStdIn (),
		   name = "<stdin>",
		   chunkSize = NONE }
    fun stdOut () =
	mkWriter { fd = SMLBasis.getStdOut (),
		   name = "<stdout>",
		   chunkSize = NONE }
    fun stdErr () =
	mkWriter { fd = SMLBasis.getStdErr (),
		   name = "<stderr>",
		   chunkSize = NONE }

    val strReader = openVector
end
