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
      | Word32
      | Real
      | Pair
      | Record
      | Ref
      | PolyVector
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

    val length : object -> int
	(* returns length part of descriptor (untagged pairs return 2);
	 * raises Representation on unboxed values.
	 *)

    exception Representation
    val toTuple  : object -> object list
    val toString : object -> string
    val toRef    : object -> object ref
    val toArray  : object -> object array
    val toVector : object -> object vector
    val toExn    : object -> exn
    val toReal   : object -> real
    val toInt    : object -> int
    val toInt32  : object -> Int32.int
    val toWord   : object -> Word.word
    val toWord8  : object -> Word8.word
    val toWord32 : object -> Word32.word

  (* fetch nth element of tuple *)
    val nth	 : (object * int) -> object

  end;


(*
 * $Log: object.sig,v $
 * Revision 1.3  1998/11/18 03:54:22  jhr
 *  New array representations.
 *
 * Revision 1.2  1998/10/28 18:24:57  jhr
 *   New Unsafe.Object API.
 *
 * Revision 1.1.1.1  1998/04/08 18:40:00  george
 * Version 110.5
 *
 *)
