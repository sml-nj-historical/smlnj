(* text-io-sig.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 * extracted from text-io.mldoc (v. 1.9; 1997-04-16)
 *)

signature TEXT_IO =
  sig
    include IMPERATIVE_IO
    structure StreamIO : TEXT_STREAM_IO
    val inputLine : instream -> string
    val outputSubstr : outstream * substring -> unit
    val openIn  : string -> instream
    val openOut : string -> outstream
    val openAppend : string -> outstream
    val openString : string -> instream
    val stdIn  : instream
    val stdOut : outstream
    val stdErr : outstream
    val print : string -> unit
    val scanStream : ((Char.char, StreamIO.instream) StringCvt.reader
                         -> ('a, StreamIO.instream) StringCvt.reader)
                       -> instream -> 'a option
    
  end
