(* os-io.sml
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 * Replacement OS.IO structure for Win32.
 * It implements a simple type of polling for file objects.
 * This file requires a runtime system supporting polling in Win32-IO.
 *)

local
    structure Word = WordImp
    structure Int = IntImp
    structure Int32 = Int32Imp
    structure Time = TimeImp
in
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

	type poll_flags = {rd : bool, wr: bool, pri: bool}
	datatype poll_desc = PollDesc of (iodesc * poll_flags)
	datatype poll_info = PollInfo of poll_desc
	
	fun pollDesc id = SOME (PollDesc (id,{rd=false,wr=false,pri=false}))
	fun pollToIODesc (PollDesc (pd,_)) = pd 

	exception Poll

	fun pollIn (PollDesc (iod,{rd,wr,pri})) = PollDesc (iod,{rd=true,wr=wr,pri=pri})
	fun pollOut (PollDesc (iod,{rd,wr,pri})) = PollDesc (iod,{rd=rd,wr=true,pri=pri})
	fun pollPri (PollDesc (iod,{rd,wr,pri})) = PollDesc (iod,{rd=rd,wr=wr,pri=true})

	local 
	    val poll' : ((word32 * word) list * (int * word) list * (Int32.int * int) option -> ((word32 * word) list * (int * word) list)) = 
		CInterface.c_function "WIN32-IO" "poll"

	    fun join (false, _, w) = w
	      | join (true, b, w) = Word.orb(w, b)
	    fun test (w, b) = (Word.andb(w, b) <> 0w0)
	    val rdBit = 0w1 and wrBit = 0w2 and priBit = 0w4

	    fun toPollInfoIO (fd,w) = PollInfo (PollDesc (OS.IO.IODesc (ref fd),{rd= test(w,rdBit),
                                                                               wr= test(w,wrBit),
                                                                               pri= test(w,priBit)}))
	    fun toPollInfoSock (i,w) = PollInfo (PollDesc (OS.IO.SockDesc (i),{rd = test(w,rdBit),
									       wr = test(w,wrBit),
									       pri = test(w,priBit)}))
	    fun fromPollDescIO (PollDesc (OS.IO.IODesc (ref w),{rd,wr,pri})) =(w,join (rd,rdBit, join (wr,wrBit, join (pri,priBit,0w0))))
	    fun fromPollDescSock (PollDesc (OS.IO.SockDesc (i),{rd,wr,pri})) = (i,join (rd,rdBit, join (wr,wrBit, join (pri,priBit,0w0))))

            (* To preserve equality, return the original PollDesc passed to poll.
             * This is cheesy, but restructuring the IODesc to no longer have a ref
             * cell is a substantial amount of work, as much of the Win32 FS basis
             * relies on mutability.
             *)
            fun findPollDescFromIO (pollIOs, (fd,w)) = let
                val desc = List.find (fn (PollDesc (OS.IO.IODesc (ref fd'),_)) => fd'=fd) pollIOs
            in
                case desc
                 of SOME f => SOME(PollInfo f)
                  | NONE => NONE
            end
	in
	    fun poll (pdl,t) = 
		let val timeout =
			case t of
			    SOME (t) =>
			    SOME (Int32.fromLarge (Time.toSeconds (t)),
				  Int.fromLarge (Time.toMicroseconds t))
			  | NONE => NONE
                    fun partDesc (PollDesc (OS.IO.IODesc (_),_)) = true
                      | partDesc (_) = false
                    val (pollIOs, pollSocks) = List.partition partDesc pdl
		    val (infoIO,infoSock) =
			poll' (List.map fromPollDescIO pollIOs,
			       List.map fromPollDescSock pollSocks,
			       timeout)
		in
		    List.@ (List.mapPartial (fn (p) => findPollDescFromIO(pollIOs,p)) infoIO,
			    List.map toPollInfoSock infoSock)
		end
	end
		    
        fun isIn (PollInfo(PollDesc(_, flgs))) = #rd flgs
        fun isOut (PollInfo(PollDesc(_, flgs))) = #wr flgs
        fun isPri (PollInfo(PollDesc(_, flgs))) = #pri flgs
	fun infoToPollDesc (PollInfo pd) = pd
    end
end

