(* os-io-sig.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 * extracted from os-io.mldoc (v. 1.7; 1998-01-07)
 *)

structure OS_IO : OS_IO = struct

    type iodesc = OS.IO.iodesc

    fun hash d = raise Fail "hash not yet implemented"
    fun compare (d1, d2) = raise Fail "compare not yet implemented"

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

    fun kind d = let
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

    fun pollDesc d = SOME (0w0, d)
    fun pollToIODesc (_, d) = d

    exception Poll

    fun pollIn (f, d) = (WordImp.orb (SMLBasis.POLL_RD, f), d)
    fun pollOut (f, d) = (WordImp.orb (SMLBasis.POLL_WR, f), d)
    fun pollPri (f, d) = (WordImp.orb (SMLBasis.POLL_ERR, f), d)

  (* polling function *)
    fun poll (pds, timeOut) =
	SMLBasis.osPoll (pds, Option.map (fn PreBasis.TIME t => t) timeOut)

    fun isIn (f, _) = WordImp.andb (f, SMLBasis.POLL_RD) <> 0w0
    fun isOut (f, _) = WordImp.andb (f, SMLBasis.POLL_WR) <> 0w0
    fun isPri (f, _) = WordImp.andb (f, SMLBasis.POLL_ERR) <> 0w0
    fun infoToPollDesc (i: poll_info) = (i: poll_desc)
end
