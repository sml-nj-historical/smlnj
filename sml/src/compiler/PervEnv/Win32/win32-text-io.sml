(* win32-text-io.sml
 *
 * COPYRIGHT (c) 1996 Bell Labs.
 *
 * The implementation of the TextIO stack on Win32 systems.
 *
 *)

structure TextIO :> TEXT_IO
    where type StreamIO.reader = TextPrimIO.reader
    where type StreamIO.writer = TextPrimIO.writer
    where type StreamIO.pos = TextPrimIO.pos
  = TextIOFn (structure OSPrimIO = Win32TextPrimIO);

(*
 * $Log: win32-text-io.sml,v $
 * Revision 1.2  1997/01/31 20:39:53  jhr
 * Replaced uses of "abstraction" with opaque signature matching.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:27  george
 *   Version 109.24
 *
 *)
