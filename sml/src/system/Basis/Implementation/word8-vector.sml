(* word8vector.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure Word8Vector : MONO_VECTOR =
  struct

    structure V = InlineT.Word8Vector

    val (op <)  = InlineT.DfltInt.<
    val (op >=) = InlineT.DfltInt.>=
    val (op +)  = InlineT.DfltInt.+

  (* unchecked access operations *)
    val unsafeUpdate = V.update
    val unsafeSub = V.sub

    type vector = V.vector
    type elem = Word8.word

    val vector0 : vector = InlineT.cast ""
    val createVec : int -> vector = InlineT.cast Assembly.A.create_s

    val maxLen = Core.max_length

    val fromList : elem list -> vector
	  = InlineT.cast CharVector.fromList
    val tabulate : (int * (int -> elem)) -> vector
	  = InlineT.cast CharVector.tabulate

    val length   = V.length
    val sub      = V.chkSub
(*
    val extract : (vector * int * int option) -> vector
	  = InlineT.cast CharVector.extract
*)
    val concat : vector list -> vector
	  = InlineT.cast CharVector.concat
    val update : vector * int * Word8.word -> vector
          = InlineT.cast CharVector.update

    fun app f vec = let
	  val len = length vec
	  fun app i = if (i < len)
		then (f (unsafeSub(vec, i)); app(i+1))
		else ()
	  in
	    app 0
	  end

    fun map f vec = (case (length vec)
	   of 0 => vector0
	    | len => let
		val newVec = createVec len
		fun mapf i = if (i < len)
		      then (unsafeUpdate(newVec, i, f(unsafeSub(vec, i))); mapf(i+1))
		      else ()
		in
		  mapf 0; newVec
		end
	  (* end case *))


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
	     of 0 => vector0
	      | len => let
		  val newVec = createVec len
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
	    else let val x = unsafeSub (vec, i)
		 in if p (i, x) then SOME (i, x) else loop (i + 1)
		 end
    in
	loop 0
    end

    fun find p vec = let
	val stop = length vec
	fun loop i =
	    if i >= stop then NONE
	    else let val x = unsafeSub (vec, i)
		 in if p x then SOME x else loop (i + 1)
		 end
    in
	loop 0
    end

    fun exists p vec = let
	val stop = length vec
	fun loop i =
	    i < stop andalso
	    (p (unsafeSub (vec, i)) orelse loop (i + 1))
    in
	loop 0
    end

    fun all p vec = let
	val stop = length vec
	fun loop i =
	    i >= stop orelse
	    (p (unsafeSub (vec, i)) andalso loop (i + 1))
    in
	loop 0
    end

    fun collate ecmp (a, b) = let
	val al = length a
	val bl = length b
	val stop = if al < bl then al else bl
	fun loop i =
	    if i >= stop then Int31Imp.compare (al, bl)
	    else case ecmp (unsafeSub (a, i), unsafeSub (b, i)) of
		     EQUAL => loop (i + 1)
		   | unequal => unequal
    in
	loop 0
    end
  end


