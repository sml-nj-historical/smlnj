(* win32.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 * Interface to Win32.
 *
 *)

structure Win32 : WIN32 =
    struct
	structure General = Win32_General
	structure FileSys = Win32_FileSys
	structure IO      = Win32_IO
	structure Process = Win32_Process
    end


(*
 * $Log: win32.sml,v $
 * Revision 1.1.1.1  1997/01/14 01:38:27  george
 *   Version 109.24
 *
 *)
