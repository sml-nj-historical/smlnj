(* os-io-sig.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 * extracted from os-io.mldoc (v. 1.7; 1998-01-07)
 *)

structure OS_IO : OS_IO = struct

    type iodesc = OS.IO.iodesc

    fun hash d = raise Fail "hash not yet implemented"
    fun compare (ref d1, ref d2) =
	IntImp.compare (SMLBasis.cmpIODesc (d1, d2), 0)

    datatype iodesc_kind = K of string

    structure Kind = struct
        val file = K "FILE"
        val dir = K "DIR"
        val symlink = K "symlink"
        val tty = K "TTY"
        val pipe = K "PIPE"
        val socket = K "SOCK"
        val device = K "DEV"
      end

    fun kind (ref d) = let
	val k = SMLBasis.ioDescKind d
    in
	if k = SMLBasis.IOD_KIND_FILE then Kind.file
	else if k = SMLBasis.IOD_KIND_DIR then Kind.dir
	else if k = SMLBasis.IOD_KIND_SYMLINK then Kind.symlink
	else if k = SMLBasis.IOD_KIND_TTY then Kind.tty
	else if k = SMLBasis.IOD_KIND_PIPE then Kind.pipe
	else if k = SMLBasis.IOD_KIND_SOCKET then Kind.socket
	else if k = SMLBasis.IOD_KIND_DEVICE then Kind.device
	else K "UNKNOWN"
    end

    type poll_desc = word * iodesc
    type poll_info = word * iodesc

    fun pollDesc dr = SOME (0w0, dr)
    fun pollToIODesc (_, dr) = dr

    exception Poll

    fun pollIn (f, d) = (WordImp.orb (SMLBasis.POLL_RD, f), d)
    fun pollOut (f, d) = (WordImp.orb (SMLBasis.POLL_WR, f), d)
    fun pollPri (f, d) = (WordImp.orb (SMLBasis.POLL_ERR, f), d)

  (* polling function *)
    fun poll (pds, timeOut) = let
	(* the low-level polling function does not expect ref cells *)
	fun strip (f, ref d) = (f, d)
	(* we must put the _original_ ref cells back into our result... *)
	fun sameAs d (_, ref d') = SMLBasis.cmpIODesc (d, d') = 0
	fun redress (f, d) =
	    case List.find (sameAs d) pds of
		SOME (_, dr) => (f, dr)
	      | NONE => raise Fail "impossible: bogus poll result"
    in
	map redress
	    (SMLBasis.osPoll (map strip pds,
			      Option.map (fn PreBasis.TIME t => t) timeOut))
    end

    fun isIn (f, _) = WordImp.andb (f, SMLBasis.POLL_RD) <> 0w0
    fun isOut (f, _) = WordImp.andb (f, SMLBasis.POLL_WR) <> 0w0
    fun isPri (f, _) = WordImp.andb (f, SMLBasis.POLL_ERR) <> 0w0
    fun infoToPollDesc (i: poll_info) = (i: poll_desc)
end
