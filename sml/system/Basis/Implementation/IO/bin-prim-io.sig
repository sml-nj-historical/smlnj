(* bin-prim-io.sig
 *
 * COPYRIGHT (c) 2007 by The Fellowship of SML/NJ
 *
 * Internal signature for the implementation of BinPrimIO
 *)

signature BIN_PRIM_IO =
  sig

    include PRIM_IO

    val openRd : string -> reader
    val openWr : string -> writer
    val openApp : string -> writer

  end
