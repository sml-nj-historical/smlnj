(* substring.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure Substring :> SUBSTRING
    where type char = PrimTypes.char
    where type string = PrimTypes.string
  = struct

    structure W = InlineT.DfltWord

    val op + = InlineT.DfltInt.+
    val op - = InlineT.DfltInt.-
    val op < = InlineT.DfltInt.<
    val op <= = InlineT.DfltInt.<=
    val op > = InlineT.DfltInt.>
    val op >= = InlineT.DfltInt.>=
    val op = = InlineT.=
    val unsafeSub = InlineT.CharVector.sub

  (* list reverse *)
    fun rev ([], l) = l
      | rev (x::r, l) = rev (r, x::l)

    type char = PrimTypes.char
    type string = PrimTypes.string
    datatype substring = SS of (string * int * int)

    fun base (SS arg) = arg

    fun string (SS arg) = PreString.unsafeSubstring arg

  (* NOTE: we use words to check the right bound so as to avoid
   * raising overflow.
   *)
    fun substring (s, i, n) =
	  if ((i < 0) orelse (n < 0)
	  orelse W.<(W.fromInt(String.size s), W.+(W.fromInt i, W.fromInt n)))
	    then raise General.Subscript
	    else SS(s, i, n)
    fun extract (s, i, NONE) = let
	  val len = String.size s
	  in
	    if ((0 <= i) andalso (i <= len))
	      then SS(s, i, len - i)
	      else raise General.Subscript
	  end
      | extract (s, i, SOME n) = substring(s, i, n)
    fun all s = SS(s, 0, String.size s)

    fun isEmpty (SS(_, _, 0)) = true
      | isEmpty _ = false

    fun getc (SS(s, i, 0)) = NONE
      | getc (SS(s, i, n)) = SOME(unsafeSub(s, i), SS(s, i+1, n-1))
    fun first (SS(s, i, 0)) = NONE
      | first (SS(s, i, n)) = SOME(unsafeSub(s, i))
    fun triml k (SS(s, i, n)) =
	  if (k < 0) then raise Subscript
	  else if (k >= n) then SS(s, i+n, 0)
	  else SS(s, i+k, n-k)
    fun trimr k (SS(s, i, n)) =
	  if (k < 0) then raise Subscript
	  else if (k >= n) then SS(s, i, 0)
	  else SS(s, i, n-k)

    fun sub (SS(s, i, n), j) =
	  if (InlineT.DfltInt.geu(j, n))
	    then raise General.Subscript
	    else unsafeSub(s, i+j)
    fun size (SS(_, _, n)) = n
    fun slice (SS(s, i, n), j, NONE) =
	  if ((0 <= j) andalso (j <= n))
	    then SS(s, i+j, n-j)
	    else raise Subscript
      | slice (SS(s, i, n), j, SOME m) =
	(* NOTE: we use words to check the right bound so as to avoid
	 * raising overflow.
	 *)
	  if ((j < 0) orelse (m < 0)
	  orelse W.<(W.fromInt n, W.+(W.fromInt j, W.fromInt m)))
	    then raise General.Subscript
	    else SS(s, i+j, m)
  (* concatenate a list of substrings together *)
    fun concat ssl = let
	fun length (len, sl, []) = (len, sl)
	  | length (len, sl, (SS(s, i, n)::rest)) =
	      length(len+n, PreString.unsafeSubstring(s, i, n)::sl, rest)
	in
	  PreString.revConcat (length (0, [], ssl))
	end
	  
  (* explode a substring into a list of characters *)
    fun explode (SS(s, i, n)) = let
	  fun f(l, j) = if (j < i)
		then l
		else f(unsafeSub(s, j) :: l, j-1)
	  in
	    f(nil, (i + n) - 1)
	  end

  (* Substring comparisons *)
    fun isPrefix s1 (SS(s2, i2, n2)) = PreString.isPrefix (s1, s2, i2, n2)
    fun compare (SS(s1, i1, n1), SS(s2, i2, n2)) =
	  PreString.cmp (s1, i1, n1, s2, i2, n2)
    fun collate cmpFn (SS(s1, i1, n1), SS(s2, i2, n2)) =
	  PreString.collate cmpFn (s1, i1, n1, s2, i2, n2)

    fun splitAt (SS(s, i, n), k) =
	  if (InlineT.DfltInt.ltu(n, k))
	    then raise Subscript
	    else (SS(s, i, k), SS(s, i+k, n-k))

    local
      fun scanl chop pred (SS(s, i, n)) = let
	    val stop = i+n
	    fun scan j = if ((j <> stop) andalso pred(unsafeSub(s, j)))
		  then scan(j+1)
		  else j
	    in
	      chop (s, i, n, scan i - i)
	    end
      fun scanr chop pred (SS(s, i, n)) = let
	    val stop = i-1
	    fun scan j = if ((j <> stop) andalso pred(unsafeSub(s, j)))
		  then scan(j-1)
		  else j
	    in
	      chop (s, i, n, (scan (i+n-1) - i) + 1)
	    end
    in
    val splitl = scanl (fn (s, i, n, k) => (SS(s, i, k), SS(s, i+k, n-k)))
    val splitr = scanr (fn (s, i, n, k) => (SS(s, i, k), SS(s, i+k, n-k)))
    val dropl  = scanl (fn (s, i, n, k) => SS(s, i+k, n-k))
    val dropr  = scanr (fn (s, i, n, k) => SS(s, i, k))
    val takel  = scanl (fn (s, i, n, k) => SS(s, i, k))
    val taker  = scanr (fn (s, i, n, k) => SS(s, i+k, n-k))
    end (* local *)
	
  (* find the position of the first occurrence of s in the substring.
   * NOTE: some day we might want to implement KMP matching for this
   *)
    fun position s (SS (s', i, n)) = let
	  val len = String.size s
	  fun eq (j, k) = (j >= len) orelse
		((unsafeSub(s, j) = unsafeSub(s', k)) andalso eq (j+1, k+1))
	  val stop = i+n-len
	  fun cmp k =
		if (k > stop) then i+n (* failure *)
		else if eq(0, k) then k
		else cmp(k+1)
	  val indx = cmp i
	  in
	    (SS(s', i, indx-i), SS(s', indx, i+n-indx))
	  end

    fun span (SS(s1, i1, n1), SS(s2, i2, n2)) =
	  if ((s1 = s2) andalso (i1 <= i2+n2))
	    then SS(s1, i1, (i2+n2)-i1)
	    else raise General.Span

    fun translate tr (SS(s, i, n)) =
	  PreString.translate (tr, s, i, n)

    fun tokens isDelim (SS(s, i, n)) = let
	  val stop = i+n
	  fun substr (i, j, l) =
		if (i = j) then l else SS(s, i, j-i)::l
	  fun scanTok (i, j, toks) = if (j < stop)
		  then if (isDelim (unsafeSub (s, j)))
		    then skipSep(j+1, substr(i, j, toks))
		    else scanTok (i, j+1, toks)
		  else substr(i, j, toks)
	  and skipSep (j, toks) = if (j < stop)
		  then if (isDelim (unsafeSub (s, j)))
		    then skipSep(j+1, toks)
		    else scanTok(j, j+1, toks)
		  else toks
	  in
	    rev (scanTok (i, i, []), [])
	  end
    fun fields isDelim (SS(s, i, n)) = let
	  val stop = i+n
	  fun substr (i, j, l) = SS(s, i, j-i)::l
	  fun scanTok (i, j, toks) = if (j < stop)
		  then if (isDelim (unsafeSub (s, j)))
		    then scanTok (j+1, j+1, substr(i, j, toks))
		    else scanTok (i, j+1, toks)
		  else substr(i, j, toks)
	  in
	    rev (scanTok (i, i, []), [])
	  end

    fun foldl f init (SS(s, i, n)) = let
	  val stop = i+n
	  fun iter (j, accum) = if (j < stop)
		then iter (j+1, f (unsafeSub(s, j), accum))
		else accum
	  in
	    iter (i, init)
	  end
    fun foldr f init (SS(s, i, n)) = let
	  fun iter (j, accum) = if (j >= i)
		then iter (j-1, f (unsafeSub(s, j), accum))
		else accum
	  in
	    iter (i+n-1, init)
	  end
    fun app f (SS(s, i, n)) = let
	  val stop = i+n
	  fun iter j = if (j < stop)
		then (f (unsafeSub(s, j)); iter (j+1))
		else ()
	  in
	    iter i
	  end

  end;

(*
 * $Log$
 *)
