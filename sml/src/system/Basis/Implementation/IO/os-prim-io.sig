(* os-prim-io.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This is an interface to a PrimIO structure augmented with OS specific
 * functions for opening readers and writers.  Used as parameter signature
 * for functor BinIOFn.
 *
 *)

signature OS_PRIM_IO =
sig
  structure PrimIO : PRIM_IO

  val openRd  : string -> PrimIO.reader
  val openWr  : string -> PrimIO.writer
  val openApp : string -> PrimIO.writer

end
