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

    val toObject : 'a -> object = InlineT.cast

    val boxed = InlineT.boxed
    val unboxed = InlineT.unboxed

    fun rep obj = if (unboxed obj)
	  then Unboxed
	  else (case (InlineT.gettag obj)
	     of 0x02 (* tag_record *) =>
		  if (InlineT.objlength obj = 2)
		    then Pair
		    else Record
	      | 0x06 (* tag_vec_hdr *) => (case (InlineT.objlength obj)
		 of 0 => PolyVector
		  | 1 => ByteVector
		  | _ => raise Fail "unknown vec_hdr"
		(* end case *))
	      | 0x0a (* tag_tag_arr_hdr *) => (case (InlineT.objlength obj)
		 of 0 => PolyArray
		  | 1 => ByteArray
		  | 6 => RealArray
		  | _ => raise Fail "unknown arr_hdr"
		(* end case *))
	      | 0x0e (* tag_arr_data/tag_ref *) =>
		  if (InlineT.objlength obj = 1)
		    then Ref
		    else raise Fail "Unknown arr_data"
	      | 0x12 (* tag_raw32 *) => Word32
	      | 0x16 (* tag_raw64 *) => Real
	      | 0x1a (* tag_special *) => (case (InlineT.getspecial obj)
		 of (0 | 1) => Susp
		  | (2 | 3) => WeakPtr
		  | _ => raise Fail "unknown special"
		(* end case *))
	      | _ (* tagless pair *) => Pair
	    (* end case *))

    exception Representation

    fun length obj = (case (rep obj)
	   of Pair => 2
	    | Unboxed => raise Representation
	    | _ => InlineT.objlength obj
	  (* end case *))

    fun nth (obj, n) = (case (rep obj)
	   of Pair =>
		if ((0 <= n) andalso (n < 2))
		  then InlineT.recordSub(obj, n)
		  else raise Representation
	    | Record => let val len = InlineT.objlength obj
		in
		  if ((0 <= n) andalso (n < len))
		    then InlineT.recordSub(obj, n)
		    else raise Representation
		end
	    | Real => let val len = InlineT.Int31.rshift(InlineT.objlength obj, 1)
		in
		  if ((n < 0) orelse (len <= n))
		    then raise Representation
		  else if (n = 0)
		    then obj	(* flat singleton tuple *)
		    else InlineT.cast(InlineT.raw64Sub(obj, n))
		end
	    | _ => raise Representation
	  (* end case *))

    fun toTuple obj = (case (rep obj)
	   of Pair => [
		  InlineT.recordSub(obj, 0),
		  InlineT.recordSub(obj, 1)
		]
	    | Record => let
		fun f i = InlineT.recordSub(obj, i)
		in
		  List.tabulate (InlineT.objlength obj, f)
		end
	    | Real => let
		val len = InlineT.Int31.rshift(InlineT.objlength obj, 1)
		fun f i = (InlineT.cast(InlineT.raw64Sub(obj, i)) : object)
		in
		  if (len = 1)
		    then [obj]
		    else List.tabulate (len, f)
		end
	    | _ => raise Representation
	  (* end case *))
    fun toString obj = (case (rep obj)
	   of ByteVector => ((InlineT.cast obj) : string)
	    | _ => raise Representation
	  (* end case *))
    fun toRef obj =
	  if (rep obj = Ref)
	    then ((InlineT.cast obj) : object ref)
	    else raise Representation
    fun toArray obj = (case (rep obj)
	   of PolyArray => ((InlineT.cast obj) : object array)
	    | _ => raise Representation
	  (* end case *))
    fun toVector obj = (case (rep obj)
	   of PolyVector => ((InlineT.cast obj) : object vector)
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
	  if (rep obj = Word32)
	    then ((InlineT.cast obj) : Int32.int)
	    else raise Representation
    fun toWord  obj = if (unboxed obj)
	  then ((InlineT.cast obj) : word)
	  else raise Representation
    fun toWord8 obj = if (unboxed obj)
	  then ((InlineT.cast obj) : Word8.word)
	  else raise Representation
    fun toWord32 obj =
	  if (rep obj = Word32)
	    then ((InlineT.cast obj) : Word32.word)
	    else raise Representation

  end;


(*
 * $Log: object.sml,v $
 * Revision 1.3  1998/11/18 03:54:24  jhr
 *  New array representations.
 *
 * Revision 1.2  1998/10/28 18:24:59  jhr
 *   New Unsafe.Object API.
 *
 * Revision 1.1.1.1  1998/04/08 18:40:01  george
 * Version 110.5
 *
 *)
