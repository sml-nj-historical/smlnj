(* list-pair.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * If lists are of unequal length, the excess elements from the
 * tail of the longer one are ignored. No exception is raised.
 *
 *)

structure ListPair : LIST_PAIR =
  struct

  exception UnequalLengths

  (* for inlining *)
    fun rev l = let 
          fun loop ([], acc) = acc
            | loop (a::r, acc) = loop(r, a::acc)
          in
	    loop (l, [])
	  end

    fun zip (l1, l2) = let
	  fun zip' ((a :: r1), (b :: r2), l) = zip' (r1, r2, (a, b)::l)
	    | zip' (_, _, l) = rev l
	  in
	    zip' (l1, l2, [])
	  end

    fun zipEq (l1, l2) = let
	fun zip' ([], [], l) = rev l
	  | zip' (x :: xs, y :: ys, l) = zip' (xs, ys, (x, y) :: l)
	  | zip' _ = raise UnequalLengths
    in
	zip' (l1, l2, [])
    end

    fun unzip l = let
	  fun unzip' ([], l1, l2) = (l1, l2)
	    | unzip' ((a, b) :: r, l1, l2) = unzip' (r, a::l1, b::l2)
	  in
	    unzip' (rev l, [], [])
	  end

    fun map f = let
	  fun mapf (a::r1, b::r2, l) = mapf (r1, r2, f(a, b) :: l)
	    | mapf (_, _, l) = rev l
	  in
	    fn (l1, l2) => mapf (l1, l2, [])
	  end

    fun mapEq f = let
	fun mapf ([], [], l) = rev l
	  | mapf (x :: xs, y :: ys, l) = mapf (xs, ys, f (x, y) :: l)
	  | mapf _ = raise UnequalLengths
    in
	fn (xs, ys) => mapf (xs, ys, [])
    end

    fun app f = let
	  fun appf (a::r1, b::r2) = (f(a, b); appf(r1, r2))
	    | appf _ = ()
	  in
	    appf
	  end

    fun appEq f = let
	fun appf ([], []) = ()
	  | appf (x :: xs, y :: ys) = (f (x, y); appf (xs, ys))
	  | appf _ = raise UnequalLengths
    in
	appf
    end

    fun all pred = let
	  fun allp (a::r1, b::r2) = pred(a, b) andalso allp (r1, r2)
	    | allp _ = true
	  in
	    allp
	  end

    fun allEq pred = let
	fun allp ([], []) = true
	  | allp (x :: xs, y :: ys) = pred (x, y) andalso allp (xs, ys)
	  | allp _ = raise UnequalLengths
    in
	allp
    end

    fun foldl f init (l1, l2) = let
	  fun foldf (x::xs, y::ys, accum) = foldf(xs, ys, f(x, y, accum))
	    | foldf (_, _, accum) = accum
	  in
	    foldf (l1, l2, init)
	  end

    fun foldlEq f init (xs, ys) = let
	fun foldlf ([], [], accum) = accum
	  | foldlf (x :: xs, y :: ys, accum) = foldlf (xs, ys, f (x, y, accum))
	  | foldlf _ = raise UnequalLengths
    in
	foldlf (xs, ys, init)
    end

    fun foldr f init = let
	  fun foldf (x::xs, y::ys) = f(x, y, foldf(xs, ys))
	    | foldf _ = init
	  in
	    foldf
	  end

    fun foldrEq f init = let
	fun foldrf ([], []) = init
	  | foldrf (x :: xs, y :: ys) = f (x, y, foldrf (xs, ys))
	  | foldrf _ = raise UnequalLengths
    in
	foldrf
    end

    fun exists pred = let
	  fun existsp (a::r1, b::r2) = pred(a, b) orelse existsp (r1, r2)
	    | existsp _ = false
	  in
	    existsp
	  end

  end (* structure ListPair *)

