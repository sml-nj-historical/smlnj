(* char-array.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Labs.
 *
 *)

structure CharArray : MONO_ARRAY =
  struct
    structure A = InlineT.CharArray
    val (op <)  = InlineT.DfltInt.<
    val (op >=) = InlineT.DfltInt.>=
    val (op +)  = InlineT.DfltInt.+

  (* unchecked access operations *)
    val unsafeUpdate = A.update
    val unsafeSub = A.sub
    val vecUpdate = InlineT.CharVector.update
    val vecSub = InlineT.CharVector.sub

    type elem = char
    type vector = string
    type array = A.array

    val maxLen = Core.max_length

    fun array (0, c) = A.array0
      | array (len, c) = if (InlineT.DfltInt.ltu(maxLen, len))
	    then raise General.Size
	    else let
	      val arr = A.create len
	      fun init i = if (i < len)
		    then (unsafeUpdate(arr, i, c); init(i+1))
		    else ()
	      in
		init 0; arr
	      end

    fun tabulate (0, _) = A.array0
      | tabulate (len, f) = if (InlineT.DfltInt.ltu(maxLen, len))
	    then raise General.Size
	    else let
	      val arr = A.create len
	      fun init i = if (i < len)
		    then (unsafeUpdate(arr, i, f i); init(i+1))
		    else ()
	      in
		init 0; arr
	      end

    fun fromList [] = A.array0
      | fromList l = let
	  fun length ([], n) = n
	    | length (_::r, n) = length (r, n+1)
	  val len = length (l, 0)
	  val _ = if (maxLen < len) then raise General.Size else ()
	  val arr = A.create len
	  fun init ([], _) = ()
	    | init (c::r, i) = (unsafeUpdate(arr, i, c); init(r, i+1))
	  in
	    init (l, 0); arr
	  end

    val length      : array -> int		  = InlineT.CharArray.length
    val sub         : (array * int) -> elem	  = InlineT.CharArray.chkSub
    val update      : (array * int * elem) -> unit 
                                               = InlineT.CharArray.chkUpdate

    fun extract (arr, base, optLen) = let
	  val len = length arr
	  fun newVec n = let
		val newV = Assembly.A.create_s n
		fun fill i = if (i < n)
		      then (vecUpdate(newV, i, unsafeSub(arr, base+i)); fill(i+1))
		      else ()
		in
		  fill 0; newV
		end
	  in
	    case (base, optLen)
	     of (0, NONE) => if (0 < len) then newVec len else ""
	      | (_, SOME 0) => if ((base < 0) orelse (len < base))
		  then raise General.Subscript
		  else ""
	      | (_, SOME 1) => String.str(sub(arr, base))
	      | (_, NONE) => if ((base < 0) orelse (len < base))
		    then raise General.Subscript
		  else if (len = base)
		    then ""
		    else newVec (len - base)
	      | (_, SOME n) =>
		  if ((base < 0) orelse (n < 0) orelse (len < (base+n)))
		    then raise General.Subscript
		    else newVec n
	  end

    fun copy {src, si, len, dst, di} = let
	  val (sstop, dstop) = let
		val srcLen = length src
		in
		  case len
		   of NONE => if ((si < 0) orelse (srcLen < si))
		        then raise Subscript
		        else (srcLen, di+srcLen-si)
		    | (SOME n) => if ((n < 0) orelse (si < 0) orelse (srcLen < si+n))
		        then raise Subscript
		        else (si+n, di+n)
		  (* end case *)
		end
	  fun copyUp (j, k) = if (j < sstop)
		then (
		  unsafeUpdate(dst, k, unsafeSub(src, j));
		  copyUp (j+1, k+1))
		else ()
	  fun copyDown (j, k) = if (si <= j)
		then (
		  unsafeUpdate(dst, k, unsafeSub(src, j));
		  copyDown (j-1, k-1))
		else ()
	  in
	    if (di < 0) orelse (length dst < dstop)
	      then raise Subscript
	    else if (si < di)
	      then copyDown (sstop-1, dstop-1)
	      else copyUp (si, di)
	  end

    fun copyVec {src, si, len, dst, di} = let
	  val (sstop, dstop) = let
		val srcLen = String.size src
		in
		  case len
		   of NONE => if ((si < 0) orelse (srcLen < si))
		        then raise Subscript
		        else (srcLen, di+srcLen-si)
		    | (SOME n) => if ((n < 0) orelse (si < 0) orelse (srcLen < si+n))
		        then raise Subscript
		        else (si+n, di+n)
		  (* end case *)
		end
	  fun copyUp (j, k) = if (j < sstop)
		then (unsafeUpdate(dst, k, vecSub(src, j)); copyUp (j+1, k+1))
		else ()
	  in
	    if ((di < 0) orelse (length dst < dstop))
	      then raise Subscript
	      else copyUp (si, di)
	  end

    fun app f arr = let
	  val len = length arr
	  fun app i = if (i < len)
		then (f (unsafeSub(arr, i)); app(i+1))
		else ()
	  in
	    app 0
	  end

    fun foldl f init arr = let
	  val len = length arr
	  fun fold (i, accum) = if (i < len)
		then fold (i+1, f (unsafeSub(arr, i), accum))
		else accum
	  in
	    fold (0, init)
	  end

    fun foldr f init arr = let
	  fun fold (i, accum) = if (i >= 0)
		then fold (i-1, f (unsafeSub(arr, i), accum))
		else accum
	  in
	    fold (length arr - 1, init)
	  end

    fun modify f arr = let
	  val len = length arr
	  fun modify' i = if (i < len)
		then (
		  unsafeUpdate(arr, i, f (unsafeSub(arr, i)));
		  modify'(i+1))
		else ()
	  in
	    modify' 0
	  end

    fun chkSlice (arr, i, NONE) = let val len = length arr
	  in
	    if (InlineT.DfltInt.ltu(len, i))
	      then raise Subscript
	      else (arr, i, len)
	  end
      | chkSlice (arr, i, SOME n) = let val len = length arr
	  in
	    if ((0 <= i) andalso (0 <= n) andalso (i+n <= len))
	      then (arr, i, i+n)
	      else raise Subscript
	  end

    fun appi f slice = let
	  val (arr, start, stop) = chkSlice slice
	  fun app i = if (i < stop)
		then (f (i, unsafeSub(arr, i)); app(i+1))
		else ()
	  in
	    app start
	  end

    fun foldli f init slice = let
	  val (arr, start, stop) = chkSlice slice
	  fun fold (i, accum) = if (i < stop)
		then fold (i+1, f (i, unsafeSub(arr, i), accum))
		else accum
	  in
	    fold (start, init)
	  end

    fun foldri f init slice = let
	  val (arr, start, stop) = chkSlice slice
	  fun fold (i, accum) = if (i >= start)
		then fold (i-1, f (i, unsafeSub(arr, i), accum))
		else accum
	  in
	    fold (stop - 1, init)
	  end

    fun modifyi f slice = let
	  val (arr, start, stop) = chkSlice slice
	  fun modify' i = if (i < stop)
		then (
		  unsafeUpdate(arr, i, f (i, unsafeSub(arr, i)));
		  modify'(i+1))
		else ()
	  in
	    modify' start
	  end

  end (* CharArray *)


(*
 * $Log$
 *)
