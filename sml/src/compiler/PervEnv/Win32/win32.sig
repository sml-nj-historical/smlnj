(* win32.sig
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
 * $Log: win32.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:40:03  george
 * Version 110.5
 *
 *)
