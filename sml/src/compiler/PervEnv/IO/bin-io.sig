(* bin-io.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature BIN_IO =
  sig
    include IMPERATIVE_IO

    val openIn     : string -> instream
    val openOut    : string -> outstream
    val openAppend : string -> outstream
  end
  where type vector = Word8Vector.vector
  and type StreamIO.vector = Word8Vector.vector
  and type StreamIO.elem = Word8.word
  and type StreamIO.pos = Position.int;

(*
 * $Log: bin-io.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:55  george
 * Version 110.5
 *
 *)
