(* stream.sml
 *
 *   Implementation of functional input stream (TextIO.StreamIO.instream)
 *   on top of readline library linked via NLFFI.
 *
 * Copyright (c) 2004 by Toyota Technological Institute at Chicago
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure ReadlineStream : sig

    val stream : TextIO.StreamIO.instream

end = struct

    structure T = TextIO
    structure S = T.StreamIO
    structure P = TextPrimIO

    val prompt = RL.prompt "rl> "

    fun rl () =	RL.rl prompt

    val closed = ref false

    fun check () =
	if !closed then raise IO.ClosedStream
	else ()

    val buffer = ref (CharVectorSlice.full "")

    fun getN n = let
	val buf = !buffer
	val sz = CharVectorSlice.length buf
    in
	if sz > 0 then
	    if sz > n then
		(buffer := CharVectorSlice.subslice (buf, n, NONE);
		 CharVectorSlice.subslice (buf, 0, SOME n))
	    else
		(buffer := CharVectorSlice.full "";
		 buf)
	else
	    case rl () of
		NONE => CharVectorSlice.full ""
	      | SOME buf => (buffer := CharVectorSlice.full (buf ^ "\n");
			     getN n)
    end

    fun readVec n = (check (); CharVectorSlice.vector (getN n))
    fun readArr asl = let
	val _ = check ()
	val (a, di, n) = CharArraySlice.base asl
	val vsl = getN n
    in
	CharArraySlice.copyVec { src = vsl, dst = a, di = di };
	CharVectorSlice.length vsl
    end

    fun block () =
	(check ();
	 if CharVectorSlice.length (!buffer) = 0 then
	     case rl () of
		 NONE => ()
	       | SOME buf => (buffer := CharVectorSlice.full (buf ^ "\n"))
	 else ())

    fun canInput () = (check (); CharVectorSlice.length (!buffer) > 0)

    val reader =
	P.augmentReader
	  (P.RD { name = "<readlineStdIn>",
		  chunkSize = 4096,
		  readVec = SOME readVec,
		  readArr = SOME readArr,
		  readVecNB = NONE,
		  readArrNB = NONE,
		  block = SOME block,
		  canInput = SOME canInput,
		  avail = fn () => NONE,
		  getPos = NONE,
		  setPos = NONE,
		  endPos = NONE,
		  verifyPos = NONE,
		  close = fn () => closed := true,
		  ioDesc = NONE })


    val stream = S.mkInstream (reader, "")
end
