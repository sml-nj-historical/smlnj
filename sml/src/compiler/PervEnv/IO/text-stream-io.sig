(* text-stream-io-sig.sml
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
 * $Log: text-stream-io-sig.sml,v $
 * Revision 1.1.1.1  1997/01/14 01:38:19  george
 *   Version 109.24
 *
 *)
