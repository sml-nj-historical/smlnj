(* object.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *)

structure Object :> UNSAFE_OBJECT =
  struct
    type object = Core.Assembly.object

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

    val toObject : 'a -> object = InlineT.cast

    val boxed = InlineT.boxed
    val unboxed = InlineT.unboxed

    fun rep obj = if (unboxed obj)
	  then Unboxed
	  else (case (InlineT.gettag obj)
	     of 0x02 (* tag_pair *) => Pair
	      | 0x06 (* tag_reald *) => Real
	      | 0x12 (* tag_special *) => (case (InlineT.getspecial obj)
		 of (0 | 1) => Susp
		  | (2 | 3) => WeakPtr
		(* end case *))
	      | 0x22 (* tag_record *) => Record
	      | 0x26 (* tag_array *) => PolyArray
	      | 0x2a (* tag_string *) => ByteVector
	      | 0x32 (* tag_bytearray *) => ByteArray
	      | 0x36 (* tag_realdarray *) => RealArray
	      | _ (* tagless pair *) => Pair
	    (* end case *))

    exception Representation

    fun toTuple obj = (case (rep obj)
	   of Pair => #[
		  InlineT.PolyVector.sub(InlineT.cast obj, 0),
		  InlineT.PolyVector.sub(InlineT.cast obj, 1)
		]
	    | Real => #[InlineT.cast obj]
	    | Record => ((InlineT.cast obj) : object vector)
	    | RealArray => let
		val arr : Real64Array.array = (InlineT.cast obj)
		fun f i = toObject(InlineT.Real64Array.sub(arr, i))
		in
		  Vector.tabulate(InlineT.Real64Array.length arr, f)
		end
	    | _ => raise Representation
	  (* end case *))
    fun toString obj = (case (rep obj)
	   of ByteVector => ((InlineT.cast obj) : string)
	    | _ => raise Representation
	  (* end case *))
    fun toRef obj =
	  if ((rep obj = PolyArray)
	  andalso (InlineT.PolyArray.length(InlineT.cast obj) = 1))
	    then ((InlineT.cast obj) : object ref)
	    else raise Representation
    fun toArray obj = (case (rep obj)
	   of PolyArray => ((InlineT.cast obj) : object array)
	    | _ => raise Representation
	  (* end case *))
    fun toExn obj =
	  if ((rep obj = Record) andalso (InlineT.objlength obj = 3))
	    then ((InlineT.cast obj) : exn)
	    else raise Representation
    fun toReal obj = (case (rep obj)
	   of Real => ((InlineT.cast obj) : real)
	    | _ => raise Representation
	  (* end case *))
    fun toInt obj = if (unboxed obj)
	  then ((InlineT.cast obj) : int)
	  else raise Representation
    fun toInt32 obj =
	  if ((rep obj = ByteVector)
	  andalso (InlineT.CharVector.length(InlineT.cast obj) = 4))
	    then ((InlineT.cast obj) : Int32.int)
	    else raise Representation
    fun toWord  obj = if (unboxed obj)
	  then ((InlineT.cast obj) : word)
	  else raise Representation
    fun toWord8 obj = if (unboxed obj)
	  then ((InlineT.cast obj) : Word8.word)
	  else raise Representation
    fun toWord32 obj =
	  if ((rep obj = ByteVector)
	  andalso (InlineT.CharVector.length(InlineT.cast obj) = 4))
	    then ((InlineT.cast obj) : Word32.word)
	    else raise Representation

  end;


(*
 * $Log$
 *)
