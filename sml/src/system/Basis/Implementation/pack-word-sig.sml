(* pack-word-sig.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 * extracted from pack-word.mldoc (v. 1.6; 2000-05-29)
 *)

signature PACK_WORD =
  sig
    val bytesPerElem : int
    val isBigEndian : bool
    val subVec  : Word8Vector.vector * int -> LargeWord.word
    val subVecX : Word8Vector.vector * int -> LargeWord.word
    val subArr  : Word8Array.array * int -> LargeWord.word
    val subArrX : Word8Array.array * int -> LargeWord.word
    val update : Word8Array.array * int * LargeWord.word -> unit
    
  end
