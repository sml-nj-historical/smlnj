(* stream-io.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature STREAM_IO =
  sig
    type vector
    type elem
    type reader
    type writer

    type instream
    type outstream

    type pos
    type out_pos

    val input       : instream -> (vector * instream)
    val input1      : instream -> (elem * instream) option
    val inputN      : (instream * int) -> (vector * instream)
    val inputAll    : instream -> (vector * instream)
    val canInput    : (instream * int) -> int option
    val closeIn     : instream -> unit
    val endOfStream : instream -> bool
    val mkInstream  : (reader * vector) -> instream
    val getReader   : instream -> (reader * vector)
    val filePosIn   : instream -> pos

    val output        : (outstream * vector) -> unit
    val output1       : (outstream * elem) -> unit
    val flushOut      : outstream -> unit
    val closeOut      : outstream -> unit
    val setBufferMode : (outstream * IO.buffer_mode) -> unit
    val getBufferMode : outstream -> IO.buffer_mode
    val mkOutstream   : (writer * IO.buffer_mode) -> outstream
    val getWriter     : outstream -> (writer * IO.buffer_mode)
    val getPosOut     : outstream -> out_pos
    val setPosOut     : out_pos -> unit
    val filePosOut    : out_pos -> pos

  end

(*
 * $Log: stream-io.sig,v $
 * Revision 1.4  1998/10/16 17:26:30  jhr
 *   Implemented multiple EOF support; changes to basis I/O API
 *
 * Revision 1.3  1998/08/17 19:21:17  george
 *   Changed the type of TextIO.StreamIO.mkInstream to eliminate the option.
 *   [appel]
 *
 *)
