(* bin-io-sig.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 * extracted from bin-io.mldoc (v. 1.5; 1997-01-09)
 *)

signature BIN_IO =
  sig
    include IMPERATIVE_IO
      where type StreamIO.vector = Word8Vector.vector
      where type StreamIO.elem = Word8.word
    val openIn  : string -> instream
    val openOut : string -> outstream
    val openAppend : string -> outstream
    
  end
