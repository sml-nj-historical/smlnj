(* os-text-prim-io.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This implements the OS generic version of the text primitive
 * IO structure.  It is implemented by a trivial translation of the
 * binary operations (see os-bin-prim-io.sml).
 *
 *)

local
    structure String = StringImp
    structure Int = IntImp
in
structure OSTextPrimIO : OS_TEXT_PRIM_IO =
struct

    structure BinPrimIO = OSBinPrimIO
    structure PrimIO = TextPrimIO

    val bufferSzB = 4096

 (* If Char.char is really Word8.word, then very efficient versions of
  * translateIn and translateOut are possible:
  *)
    val translateIn : BinPrimIO.PrimIO.reader -> PrimIO.reader = InlineT.cast
    val translateOut : BinPrimIO.PrimIO.writer -> PrimIO.writer = InlineT.cast

    fun openRd fname = translateIn(BinPrimIO.openRd fname)
    fun openWr fname = translateOut(BinPrimIO.openWr fname)
    fun openApp fname = translateOut(BinPrimIO.openApp fname)

    fun mkReader args = translateIn(BinPrimIO.mkReader args)
    fun mkWriter args = translateOut(BinPrimIO.mkWriter args)

    fun stdIn () = mkReader{
	    fd		= BinPrimIO.mkFileDesc(SMLBasis.getStdIn()),
	    name	= "<stdIn>",
	    initBlkMode	= true (* Bug!  Should check! *)
	  }

    fun stdOut () = mkWriter{
	    fd		= BinPrimIO.mkFileDesc(SMLBasis.getStdOut()),
	    name	= "<stdOut>",
	    initBlkMode	= true (* Bug!  Should check! *),
	    appendMode	= false (* Bug!  Should check! *),
	    chunkSize	= bufferSzB
	  }

    fun stdErr () = mkWriter{
	    fd		= BinPrimIO.mkFileDesc(SMLBasis.getStdErr()),
	    name	= "<stdErr>",
	    initBlkMode	= true, (* Bug!  Should check! *)
	    appendMode	= false, (* Bug!  Should check! *)
	    chunkSize	= bufferSzB
	  }

    fun strReader src = let
	  val pos = ref 0
	  val closed = ref false
	  fun checkClosed () = if !closed then raise IO.ClosedStream else ()
	  val len = String.size src
	  fun avail () = (len - !pos)
	  fun readV n = let
		val p = !pos
		val m = Int.min(n, len-p)
		in
		  checkClosed ();
		  pos := p+m;
(** NOTE: could use unchecked operations here **)
		  String.substring (src, p, m)
		end
	  fun readA {buf, i, sz} = let
		val p = !pos
		val m = (case sz
		       of NONE => Int.min(CharArray.length buf-i, len-p)
			| (SOME n) => Int.min(n, len-p)
		      (* end case *))
		in
		  checkClosed ();
		  pos := p+m;
		  CharArray.copyVec {src=src, si=p, len=SOME m, dst=buf, di=i};
		  m
		end
	  fun getPos () = (checkClosed(); !pos)
	  in
	    PrimIO.RD{
		name      = "<string>", 
		chunkSize = len,
		readVec   = SOME(readV),
        	readArr   = SOME(readA),
		readVecNB = SOME(SOME o readV),
		readArrNB = SOME(SOME o readA),
		block     = SOME(checkClosed),
		canInput  = SOME(fn () => (checkClosed(); true)),
		avail     = SOME o avail,
		getPos    = SOME getPos,
		setPos    = SOME(fn i => (
				checkClosed();
				if (i < 0) orelse (len < i)
				  then raise Subscript
				  else ();
				pos := i)),
        	endPos    = SOME(fn () => (checkClosed(); len)),
		verifyPos = SOME getPos,
		close     = fn () => closed := true,
		ioDesc    = NONE
	      }
	  end

  end (* OSTextPrimIO *)
end