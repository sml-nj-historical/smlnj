(* vector.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 *)

structure Vector : VECTOR =
  struct

    val (op +)  = InlineT.DfltInt.+
    val (op <)  = InlineT.DfltInt.<
    val (op >=) = InlineT.DfltInt.>=

    type 'a vector = 'a PrimTypes.vector

    val maxLen = Core.max_length

    fun checkLen n = if InlineT.DfltInt.ltu(maxLen, n) then raise General.Size else ()

    fun fromList l = let
	  fun len ([], n) = n
	    | len ([_], n) = n+1
	    | len (_::_::r, n) = len(r, n+2)
	  val n = len (l, 0)
	  in
	    checkLen n;
	    if (n = 0)
	      then Assembly.vector0
	      else Assembly.A.create_v(n, l)
	  end

    fun tabulate (0, _) = Assembly.vector0
      | tabulate (n, f) = let
	  val _ = checkLen n
	  fun tab i = if (i = n) then [] else (f i)::tab(i+1)
	  in
	    Assembly.A.create_v(n, tab 0)
	  end

    val length : 'a vector -> int = InlineT.PolyVector.length
    val sub : 'a vector * int -> 'a = InlineT.PolyVector.chkSub

  (* a utility function *)
    fun rev ([], l) = l
      | rev (x::r, l) = rev (r, x::l)

    fun extract (v, base, optLen) = let
	  val len = length v
	  fun newVec n = let
		fun tab (~1, l) = Assembly.A.create_v(n, l)
		  | tab (i, l) = tab(i-1, InlineT.PolyVector.sub(v, base+i)::l)
		in
		  tab (n-1, [])
		end
	  in
	    case (base, optLen)
	     of (0, NONE) => v
	      | (_, SOME 0) => if ((base < 0) orelse (len < base))
		  then raise General.Subscript
		  else Assembly.vector0
	      | (_, NONE) => if ((base < 0) orelse (len < base))
		    then raise General.Subscript
		  else if (len = base)
		    then Assembly.vector0
		    else newVec (len - base)
	      | (_, SOME n) =>
		  if ((base < 0) orelse (n < 0) orelse (len < (base+n)))
		    then raise General.Subscript
		    else newVec n
	    (* end case *)
	  end

    fun concat [v] = v
      | concat vl = let
	(* get the total length and flatten the list *)
	  fun len ([], n, l) = (checkLen n; (n, rev(l, [])))
	    | len (v::r, n, l) = let
		val n' = InlineT.PolyVector.length v
		fun explode (i, l) = if (i < n')
		      then explode(i+1, InlineT.PolyVector.sub(v, i)::l)
		      else l
		in
		  len (r, n + n', explode(0, l))
		end
	  in
	    case len (vl, 0, [])
	     of (0, _) => Assembly.vector0
	      | (n, l) => Assembly.A.create_v(n, l)
	    (* end case *)
	  end

    fun app f vec = let
	  val len = length vec
	  fun app i = if (i < len)
		then (f (InlineT.PolyVector.sub(vec, i)); app(i+1))
		else ()
	  in
	    app 0
	  end

    fun map f vec = let
	  val len = length vec
	  fun mapf (i, l) = if (i < len)
		then mapf (i+1, f (InlineT.PolyVector.sub(vec, i)) :: l)
		else Assembly.A.create_v(len, rev(l, []))
	  in
	    if (len > 0)
	      then mapf (0, [])
	      else Assembly.vector0
	  end

    fun foldl f init vec = let
	  val len = length vec
	  fun fold (i, accum) = if (i < len)
		then fold (i+1, f (InlineT.PolyVector.sub(vec, i), accum))
		else accum
	  in
	    fold (0, init)
	  end

    fun foldr f init vec = let
	  fun fold (i, accum) = if (i >= 0)
		then fold (i-1, f (InlineT.PolyVector.sub(vec, i), accum))
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

    fun appi f slice = let
	  val (vec, start, stop) = chkSlice slice
	  fun app i = if (i < stop)
		then (f (i, InlineT.PolyVector.sub(vec, i)); app(i+1))
		else ()
	  in
	    app start
	  end

    fun mapi f slice = let
	  val (vec, start, stop) = chkSlice slice
	  val len = stop-start
	  fun mapf (i, l) = if (i < stop)
		then mapf (i+1, f (i, InlineT.PolyVector.sub(vec, i)) :: l)
		else Assembly.A.create_v(len, rev(l, []))
	  in
	    if (len > 0)
	      then mapf (start, [])
	      else Assembly.vector0
	  end

    fun foldli f init slice = let
	  val (vec, start, stop) = chkSlice slice
	  fun fold (i, accum) = if (i < stop)
		then fold (i+1, f (i, InlineT.PolyVector.sub(vec, i), accum))
		else accum
	  in
	    fold (start, init)
	  end

    fun foldri f init slice = let
	  val (vec, start, stop) = chkSlice slice
	  fun fold (i, accum) = if (i >= start)
		then fold (i-1, f (i, InlineT.PolyVector.sub(vec, i), accum))
		else accum
	  in
	    fold (stop - 1, init)
	  end

  end  (* Vector *)


(*
 * $Log$
 *)
