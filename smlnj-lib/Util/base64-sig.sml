(* base64-sig.sml
 *
 * COPYRIGHT (c) 2012 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Support for Base64 encoding/decoding as specified by RFC 4648.
 *
 *	http://www.ietf.org/rfc/rfc4648.txt
 *)

signature BASE64 =
  sig

    val encodeVec : Word8Vector.vector -> string
    val encodeVecSlice : Word8VectorSlice.slice -> string

  end
