(* bin-io.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *
 * The implementation of the BinIO stack on Posix systems.
 *
 *)

structure BinIO :> BIN_IO
    where type StreamIO.reader = BinPrimIO.reader
    where type StreamIO.writer = BinPrimIO.writer
(*    where type StreamIO.pos = BinPrimIO.pos  - redundant *)
  = BinIOFn (structure OSPrimIO = PosixBinPrimIO);

(*
 * $Log: posix-bin-io.sml,v $
 * Revision 1.3  1997/07/15 15:54:07  dbm
 *   Removed redundant where type definition.
 *
 * Revision 1.2  1997/01/31  20:39:50  jhr
 * Replaced uses of "abstraction" with opaque signature matching.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:25  george
 *   Version 109.24
 *
 *)
