(* object.sig
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *)

signature UNSAFE_OBJECT =
  sig
    type object

  (* information about the memory representation of an object.
   * NOTE: some of these are not supported yet, but will be once the new
   * array representation is available.
   *)
    datatype representation
      = Unboxed
      | Real
      | Pair
      | Record
(*      | PolyVector	use Record for now *)
      | PolyArray	(* includes ref *)
      | ByteVector	(* includes Word8Vector.vector and CharVector.vector *)
      | ByteArray	(* includes Word8Array.array and CharArray.array *)
(*      | RealVector	use PolyVector for now *)
      | RealArray
      | Susp
      | WeakPtr

    val toObject : 'a -> object

    val boxed : object -> bool
    val unboxed : object -> bool
    val rep : object -> representation

    exception Representation
    val toTuple  : object -> object vector
    val toString : object -> string
    val toRef    : object -> object ref
    val toArray  : object -> object array
    val toExn    : object -> exn
    val toReal   : object -> real
    val toInt    : object -> int
    val toInt32  : object -> Int32.int
    val toWord   : object -> Word.word
    val toWord8  : object -> Word8.word
    val toWord32 : object -> Word32.word

  end;


(*
 * $Log: object.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:40:00  george
 * Version 110.5
 *
 *)
