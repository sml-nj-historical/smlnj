(* win32-general.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 * General Win32 stuff.
 *
 *)

structure Win32_General : WIN32_GENERAL = 
    struct
	structure Word = Word32
	type word = Word.word

	type hndl = word
	type system_time = {year: int,
			    month: int,
			    dayOfWeek: int,
			    day: int,
			    hour: int,
			    minute: int,
			    second: int,
			    milliSeconds: int}
	
	val arcSepChar = #"\\"

	local
	    fun cfun' lib name = CInterface.c_function lib name
	    val sayDebug' : string -> unit = cfun' "WIN32" "debug"
	in
	    val sayDebug = (* sayDebug' *) fn _ => ()
	    val log : string list ref = ref []
	    fun logMsg s = (log := s :: (!log);
			    sayDebug s)
	    fun cfun lib name = 
		(logMsg ("binding C function <"^lib^":"^name^">...");
		 cfun' lib name
		   before
		 logMsg "bound\n")
	end

	val getConst' : (string * string) -> word = 
	    cfun "WIN32" "get_const"
	fun getConst kind name = getConst'(kind,name)

	val getLastError : unit -> word = cfun "WIN32" "get_last_error"

	val INVALID_HANDLE_VALUE = getConst "GENERAL" "INVALID_HANDLE_VALUE"
	fun isValidHandle (h : word) = h <> INVALID_HANDLE_VALUE

    end

(*
 * $Log: win32-general.sml,v $
 * Revision 1.2  1997/01/31 20:13:14  lorenz
 * turned off debug messages
 *
 * Revision 1.1.1.1  1997/01/14  01:38:27  george
 *   Version 109.24
 *
 *)
