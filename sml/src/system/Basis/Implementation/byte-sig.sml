(* byte-sig.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 * extracted from byte.mldoc (v. 1.4; 1996-08-09)
 *)

signature BYTE =
  sig
    val byteToChar : Word8.word -> char
    val charToByte : char -> Word8.word
    val bytesToString : Word8Vector.vector -> string
    val stringToBytes : string -> Word8Vector.vector
    val unpackStringVec : Word8Vector.vector * int * int option -> string
    val unpackString : Word8Array.array * int * int option -> string
    val packString : Word8Array.array * int * substring -> unit
    
  end
