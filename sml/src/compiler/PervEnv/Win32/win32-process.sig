(* win32-process-sig.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 * Signature for hooks to Win32 Process functions.
 *
 *)

signature WIN32_PROCESS = 
    sig
	val system' : string -> Win32_General.word
	val exitProcess : Win32_General.word -> 'a
	val getEnvironmentVariable' : string -> string option
    end

(*
 * $Log: win32-process-sig.sml,v $
 * Revision 1.1.1.1  1997/01/14 01:38:27  george
 *   Version 109.24
 *
 *)
