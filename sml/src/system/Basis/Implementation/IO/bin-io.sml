(* bin-io.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 * COPYRIGHT (c) 2001 Lucent Technologies, Bell Labs
 *
 * The implementation of the BinIO stack
 *
 *)
structure BinIO :> BIN_IO
    where type StreamIO.reader = BinPrimIO.reader
    where type StreamIO.writer = BinPrimIO.writer
(*    where type StreamIO.pos = BinPrimIO.pos  - redundant *)
  = BinIOFn (structure PrimIO = BinPrimIO);
