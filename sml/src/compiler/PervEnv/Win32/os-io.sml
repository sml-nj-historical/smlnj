(* win32-os-io.sml
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 * Replacement of OS.IO structure for Win32.
 * It implements a simple type of polling for file objects.
 *)

structure OS_IO : OS_IO = 
    struct
	structure W32G = Win32_General
	structure W32FS = Win32_FileSys
	type word32 = Word32.word

	exception SysErr = Assembly.SysErr

	type iodesc = OS.IO.iodesc (* IODesc of W32G.hndl ref *) 

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
	val noPolling = "polling not implemented for win32 for this device/type"

	datatype poll_desc = PollDesc of iodesc
	datatype poll_info = PollInfo of poll_desc
	
	fun pollDesc id = SOME (PollDesc id) (* NONE *)
	fun pollToIODesc (PollDesc pd) = pd (* raise Fail("pollToIODesc: "^noPolling) *)
	exception Poll

	fun pollIn pd = pd (* raise Fail("pollIn: "^noPolling) *)
	fun pollOut pd = pd (* raise Fail("pollOut: "^noPolling) *)
	fun pollPri pd = pd (* raise Fail("pollPri: "^noPolling) *)

	local 
	    val poll' : (word32 list * (Int32.int * int) option -> word32 list) = 
		CInterface.c_function "WIN32-IO" "poll"
	    fun toPollInfo (w) = PollInfo (PollDesc (OS.IO.IODesc (ref w)))
	    fun fromPollDesc (PollDesc (OS.IO.IODesc (ref w))) = w
	in
	    fun poll (pdl,t) = 
		let val timeout = (case t
				     of SOME (t) => SOME (Time.toSeconds (t),
							  Int.fromLarge (Time.toMicroseconds t))
				      | NONE => NONE)
		    val info = poll' (List.map fromPollDesc pdl,timeout)
		in
		    List.map toPollInfo info
		end
	end
		    
	fun isIn pd = raise Fail("isIn: "^noPolling)
	fun isOut pd = raise Fail("isOut: "^noPolling)
	fun isPri pd = raise Fail("isPri: "^noPolling)

	fun infoToPollDesc (PollInfo pd) = pd (* raise Fail("infoToPollDesc: "^noPolling) *)
    end
