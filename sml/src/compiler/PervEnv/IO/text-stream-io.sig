(* text-stream-io.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature TEXT_STREAM_IO =
  sig
    include STREAM_IO
    val inputLine    : instream -> (string * instream)
    val outputSubstr : (outstream * substring) -> unit
  end

(*
 * $Log: text-stream-io.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:55  george
 * Version 110.5
 *
 *)
