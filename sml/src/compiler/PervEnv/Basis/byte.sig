(* byte.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature BYTE =
  sig

    val byteToChar : Word8.word -> char
    val charToByte : char -> Word8.word

    val bytesToString : Word8Vector.vector -> string
    val stringToBytes : string -> Word8Vector.vector

    val unpackStringVec : (Word8Vector.vector * int * int option) -> string
    val unpackString    : (Word8Array.array * int * int option) -> string
    val packString      : (Word8Array.array * int * Substring.substring) -> unit

  end

(*
 * $Log: byte.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:40:04  george
 * Version 110.5
 *
 *)
