(* text-prim-io.sig
 *
 * COPYRIGHT (c) 2007 by The Fellowship of SML/NJ
 *
 * Internal signature for the implementation of TextPrimIO
 *)

signature TEXT_PRIM_IO = sig

    include PRIM_IO

    val stdIn  : unit -> reader
    val stdOut : unit -> writer
    val stdErr : unit -> writer

    val openRd : string -> reader
    val openWr : string -> writer
    val openApp : string -> writer

    val strReader : string -> reader

end
