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

    fun mkWriter { fd, name, appendMode, chunkSize } =
	raise Fail "mkWriter not yet implemented"

    fun stdIn () =
	mkReader { fd = SMLBasis.getStdIn (),
		   name = "<stdin>",
		   chunkSize = NONE }
    fun stdOut () =
	mkWriter { fd = SMLBasis.getStdOut (),
		   name = "<stdout>",
		   appendMode = false,
		   chunkSize = NONE }
    fun stdErr () =
	mkWriter { fd = SMLBasis.getStdErr (),
		   name = "<stderr>",
		   appendMode = true,
		   chunkSize = NONE }

    val strReader = openVector
end
