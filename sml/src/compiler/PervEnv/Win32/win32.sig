(* win32-sig.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 * Signature for the interface to Win32.
 *
 *)

signature WIN32 =
    sig
	structure General : WIN32_GENERAL
	structure FileSys : WIN32_FILESYS
	structure IO      : WIN32_IO
	structure Process : WIN32_PROCESS
    end 


(*
 * $Log: win32-sig.sml,v $
 * Revision 1.1.1.1  1997/01/14 01:38:27  george
 *   Version 109.24
 *
 *)
