(* os-io.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 * Win32 implementation of the OS.IO structure.
 *
 *)

structure OS_IO : OS_IO = 
    struct
	structure W32G = Win32_General
	structure W32FS = Win32_FileSys

	exception SysErr = Assembly.SysErr

	type iodesc = OS.IO.iodesc

	(* hash: can't assume 32 bits *)
	fun hash (OS.IO.IODesc (ref (0wxffffffff : W32G.hndl))) = 
	    0wx7fffffff : word 
	  | hash (OS.IO.IODesc (ref h)) = (Word.fromInt o W32G.Word.toInt) h

	fun compare (OS.IO.IODesc (ref wa),OS.IO.IODesc (ref wb)) = 
	    W32G.Word.compare(wa,wb)

        datatype iodesc_kind = K of string

	structure Kind =
	    struct
		val file = K "FILE"
		val dir = K "DIR"
		val symlink = K "LINK"
		val tty = K "TTY"
		val pipe = K "PIPE"
		val socket = K "SOCK"
		val device = K "DEV"
	    end

	fun kind (OS.IO.IODesc (ref h)) = 
	    case W32FS.getFileAttributes' h of
		NONE => 
		    K "UNKNOWN"
	      | SOME w =>
		    if W32FS.isRegularFile h then Kind.file
		    else Kind.dir

        (* no win32 polling devices for now *)
	val noPolling = "polling not implemented for win32"

	type poll_desc = unit
	type poll_info = unit
	
	fun pollDesc id = NONE : poll_desc option
	fun pollToIODesc pd = raise Fail("pollToIODesc: "^noPolling)
	exception Poll

	fun pollIn pd = raise Fail("pollIn: "^noPolling)
	fun pollOut pd = raise Fail("pollOut: "^noPolling)
	fun pollPri pd = raise Fail("pollPri: "^noPolling)

	fun poll (pdl,t) = raise Fail("poll: "^noPolling)

	fun isIn pd = raise Fail("isIn: "^noPolling)
	fun isOut pd = raise Fail("isOut: "^noPolling)
	fun isPri pd = raise Fail("isPri: "^noPolling)

	fun infoToPollDesc pi = raise Fail("infoToPollDesc: "^noPolling)
    end


(*
 * $Log: os-io.sml,v $
 * Revision 1.2  1997/06/02 19:16:29  jhr
 *   SML'97 Basis Library changes (phase 2)
 *
 * Revision 1.1.1.1  1997/01/14  01:38:26  george
 *   Version 109.24
 *
 *)
