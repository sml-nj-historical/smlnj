(* char-vector.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * Vectors of characters (aka strings).
 *
 *)

structure CharVector : MONO_VECTOR =
  struct

    structure String = StringImp

    val (op <)  = InlineT.DfltInt.<
    val (op >=) = InlineT.DfltInt.>=
    val (op +)  = InlineT.DfltInt.+

  (* unchecked access operations *)
    val unsafeUpdate = InlineT.CharVector.update
    val unsafeSub = InlineT.CharVector.sub

    type elem = char
    type vector = string

    val maxLen = String.maxSize

    val fromList = String.implode

    fun tabulate (0, _) = ""
      | tabulate (n, f) = let
	  val _ = if (InlineT.DfltInt.ltu(maxLen, n)) then raise General.Size else ()
	  val ss = Assembly.A.create_s n
	  fun fill i = if (i < n)
		then (unsafeUpdate(ss, i, f i); fill(i+1))
		else ()
	  in
	    fill 0; ss
	  end

    val length   = InlineT.CharVector.length
    val sub      = InlineT.CharVector.chkSub

    fun update (v, i, x) = let
	fun mostlySame j =
	    if i = j then x
	    else InlineT.CharVector.sub (v, j)
    in
	tabulate (length v, mostlySame)
    end

    val extract  = String.extract
    val concat   = String.concat

    fun app f vec = let
	  val len = length vec
	  fun app i = if (i < len)
		then (f (unsafeSub(vec, i)); app(i+1))
		else ()
	  in
	    app 0
	  end

    val map = String.map

    fun foldl f init vec = let
	  val len = length vec
	  fun fold (i, accum) = if (i < len)
		then fold (i+1, f (unsafeSub(vec, i), accum))
		else accum
	  in
	    fold (0, init)
	  end

    fun foldr f init vec = let
	  fun fold (i, accum) = if (i >= 0)
		then fold (i-1, f (unsafeSub(vec, i), accum))
		else accum
	  in
	    fold (length vec - 1, init)
	  end

    fun chkSlice (vec, i, NONE) = let val len = length vec
	  in
	    if (InlineT.DfltInt.ltu(len, i))
	      then raise Subscript
	      else (vec, i, len)
	  end
      | chkSlice (vec, i, SOME n) = let val len = length vec
	  in
	    if ((0 <= i) andalso (0 <= n) andalso (i+n <= len))
	      then (vec, i, i+n)
	      else raise Subscript
	  end

    fun appi f vec = let
	  val stop = length vec
	  fun app i = if (i < stop)
		then (f (i, unsafeSub(vec, i)); app(i+1))
		else ()
	  in
	    app 0
	  end

(*
    fun mapi f slice = let
	  val (vec, start, stop) = chkSlice slice
	  in
	    case (stop - start)
	     of 0 => ""
	      | len => let
		  val newVec = Assembly.A.create_s len
		  fun mapf (i, j) = if (i < len)
			then (
			  unsafeUpdate(newVec, i, f(j, unsafeSub(vec, j)));
			  mapf(i+1, j+1))
			else ()
		  in
		    mapf (0, start); newVec
		  end
	    (* end case *)
	  end
*)
    fun mapi f vec =
	tabulate (length vec, fn i => f (i, unsafeSub (vec, i)))

    fun foldli f init vec = let
	  val stop = length vec
	  fun fold (i, accum) = if (i < stop)
		then fold (i+1, f (i, unsafeSub(vec, i), accum))
		else accum
	  in
	    fold (0, init)
	  end

    fun foldri f init vec = let
	  val stop = length vec
	  fun fold (i, accum) = if (i >= 0)
		then fold (i-1, f (i, unsafeSub(vec, i), accum))
		else accum
	  in
	    fold (stop - 1, init)
	  end

    fun findi p vec = let
	val stop = length vec
	fun loop i =
	    if i >= stop then NONE
	    else let val x = InlineT.CharVector.sub (vec, i)
		 in if p (i, x) then SOME (i, x) else loop (i + 1)
		 end
    in
	loop 0
    end

    fun find p vec = let
	val stop = length vec
	fun loop i =
	    if i >= stop then NONE
	    else let val x = InlineT.CharVector.sub (vec, i)
		 in if p x then SOME x else loop (i + 1)
		 end
    in
	loop 0
    end

    fun exists p vec = let
	val stop = length vec
	fun loop i =
	    i < stop andalso
	    (p (InlineT.CharVector.sub (vec, i)) orelse loop (i + 1))
    in
	loop 0
    end

    fun all p vec = let
	val stop = length vec
	fun loop i =
	    i >= stop orelse
	    (p (InlineT.CharVector.sub (vec, i)) andalso loop (i + 1))
    in
	loop 0
    end

    fun collate ecmp (a, b) = let
	val al = length a
	val bl = length b
	val stop = if al < bl then al else bl
	fun loop i =
	    if i >= stop then Int31Imp.compare (al, bl)
	    else case ecmp (InlineT.CharVector.sub (a, i),
			    InlineT.CharVector.sub (b, i)) of
		     EQUAL => loop (i + 1)
		   | unequal => unequal
    in
	loop 0
    end
  end (* CharVector *)


