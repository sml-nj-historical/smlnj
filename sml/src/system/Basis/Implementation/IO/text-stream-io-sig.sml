(* text-stream-io-sig.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 * extracted from text-stream-io.mldoc (v. 1.0; 1996-04-15)
 *)

signature TEXT_STREAM_IO =
  sig
    include STREAM_IO
      where type vector = CharVector.vector
      where type elem = Char.char
    val inputLine : instream -> string * instream
    val outputSubstr : outstream * substring -> unit
    
  end
