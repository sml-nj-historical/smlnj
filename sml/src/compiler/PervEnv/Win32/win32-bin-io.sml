(* win32-bin-io.sml
 *
 * COPYRIGHT (c) 1996 Bell Labs.
 *
 * The implementation of the BinIO stack on Win32 systems.
 *
 *)

structure BinIO :> BIN_IO
    where type StreamIO.reader = BinPrimIO.reader
    where type StreamIO.writer = BinPrimIO.writer
(**    where type StreamIO.pos = BinPrimIO.pos  **)
  = BinIOFn (structure OSPrimIO = Win32BinPrimIO);


(*
 * $Log: win32-bin-io.sml,v $
 * Revision 1.3  1997/07/27 20:35:22  lorenz
 * minor 'where type' change
 *
 * Revision 1.2  1997/01/31  20:39:52  jhr
 * Replaced uses of "abstraction" with opaque signature matching.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:26  george
 *   Version 109.24
 *
 *)
