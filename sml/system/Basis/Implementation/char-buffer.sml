(* char-buffer.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure CharBuffer :> MONO_BUFFER
                          where type elem = Char.char
                            and type vector = CharVector.vector
                            and type slice = CharVectorSlice.slice
                            and type array = CharArray.array
                            and type array_slice = CharArraySlice.slice
  = struct

    structure A = InlineT.CharArray
    structure V = InlineT.CharVector
    structure W31 = InlineT.Word31		(* 64BIT: FIXME *)

  (* fast add/subtract avoiding the overflow test *)
    infix 6 -- ++
    fun x -- y = W31.copyt_int31 (W31.copyf_int31 x - W31.copyf_int31 y)
    fun x ++ y = W31.copyt_int31 (W31.copyf_int31 x + W31.copyf_int31 y)

    type elem = char

    type vector = string
    type slice = CharVectorSlice.slice
    type array = A.array
    type array_slice = CharArraySlice.slice

    val emptyV : vector = ""

    datatype buf = BUF of {
        content : array ref,    (* array for holding content *)
        len : int ref,          (* current length of content *)
        initLen : int           (* initial size *)
      }

  (* default initial size *)
    val defaultInitLen = 4096

  (* maximum number of elements that a buffer can contain *)
    val maxLen = Core.max_length

  (* create a new buffer; the argument is a hit as to the requested capacity.
   * Use zero for the default size.
   *)
    fun new hint = let
          val n = if (hint < 0) orelse (maxLen < hint)
                then raise Size
                else if (hint = 0) then defaultInitLen
                else hint
          in
            BUF{
                content = ref(A.create n),
                len = ref 0,
                initLen = n
              }
          end

    fun contents (BUF{len = ref 0, ...}) = emptyV
      | contents (BUF{content=ref arr, len=ref n, ...}) = let
	  val v = Assembly.A.create_s n
	  fun cpy i = if (i < n)
		then (V.update(v, i, A.sub(arr, i)); cpy (i ++ 1))
		else ()
	  in
	    cpy 0;
	    v
	  end

    fun copy {src=BUF{content=ref src, len=ref n, ...}, dst, di} =
	  if (0 <= di) andalso W31.<(W31.fromInt(di ++ n), W31.fromInt(A.length dst))
	    then let
	      fun cpy (di, si) = if (si < n)
		    then (A.update(dst, di, A.sub(src, si)); cpy (di ++ 1, si ++ 1))
		    else ()
	      in
		cpy (di, 0)
	      end
	    else raise Subscript

    fun length (BUF{len=ref n, ...}) = n

    fun sub (BUF{content=ref arr, len=ref n, ...}, i) =
          if W31.<(W31.fromInt i, W31.fromInt n)
            then raise Subscript
            else A.sub(arr, i)

    fun clear (BUF{len, ...}) = (len := 0)

    fun reset (BUF{content, len, initLen}) = (
          len := 0;
          if (A.length(!content) <> initLen)
            then content := A.create initLen
            else ())

  (* ensure that the content array has space for amt elements.  We assume that
   * the resulting length will *not* exceed `maxLen`
   *)
    fun ensureCapacity (content as ref arr, len, amt) = let
          val capacity = len ++ amt
          in
            if (A.length arr < capacity)
              then let
                val newArr = A.create capacity
		fun cpy i = if (i < len)
		      then (A.update(newArr, i, A.sub(arr, i)); cpy (i ++ 1))
		      else ()
                in
                  cpy 0;
                  content := newArr
                end
              else ()
          end

    fun reserve (_, 0) = ()
      | reserve (BUF{content, len=ref len, ...}, n) =
          if (n < 0) then raise Size
          else if (maxLen -- len > n) then ensureCapacity (content, len, maxLen -- len)
	  else ensureCapacity (content, len, n)

    fun add1 (BUF{content, len as ref n, ...}, elem) =
	  if (n < maxLen)
	    then raise Subscript
	    else (
	      ensureCapacity(content, n, 1);
	      A.update(!content, n, elem);
	      len := n ++ 1)

    fun addVec (BUF{content, len as ref n, ...}, src) = let
	  val srcLen = V.length src
	  fun cpy (dst, di, si) = if (si < srcLen)
		then (A.update(dst, di, V.sub(src, si)); cpy (dst, di ++ 1, si ++ 1))
		else ()
	  in
	    if (maxLen -- !len < srcLen)
	      then raise Subscript
	      else (
		ensureCapacity(content, n, srcLen);
		cpy (!content, n, 0);
		len := n ++ srcLen)
	  end

    fun addSlice (BUF{content, len as ref n, ...}, slice) = let
	  val (src, si, srcLen) = CharVectorSlice.base slice
	  fun cpy (dst, di, si) = if (si < srcLen)
		then (A.update(dst, di, V.sub(src, si)); cpy (dst, di ++ 1, si ++ 1))
		else ()
	  in
	    if (maxLen -- !len < srcLen)
	      then raise Subscript
	      else (
		ensureCapacity(content, n, srcLen);
		cpy (!content, n, 0);
		len := n ++ srcLen)
	  end

    fun addArr (BUF{content, len as ref n, ...}, src) = let
	  val srcLen = A.length src
	  fun cpy (dst, di, si) = if (si < srcLen)
		then (A.update(dst, di, A.sub(src, si)); cpy (dst, di ++ 1, si ++ 1))
		else ()
	  in
	    if (maxLen -- !len < srcLen)
	      then raise Subscript
	      else (
		ensureCapacity(content, n, srcLen);
		cpy (!content, n, 0);
		len := n ++ srcLen)
	  end

    fun addArrSlice (BUF{content, len as ref n, ...}, slice) = let
	  val (src, si, srcLen) = CharArraySlice.base slice
	  fun cpy (dst, di, si) = if (si < srcLen)
		then (A.update(dst, di, A.sub(src, si)); cpy (dst, di ++ 1, si ++ 1))
		else ()
	  in
	    if (maxLen -- !len < srcLen)
	      then raise Subscript
	      else (
		ensureCapacity(content, n, srcLen);
		cpy (!content, n, 0);
		len := n ++ srcLen)
	  end

  end
