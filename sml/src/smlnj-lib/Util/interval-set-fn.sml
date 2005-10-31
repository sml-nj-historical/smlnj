(* interfun-set-fn.sml
 *
 * COPYRIGHT (c) 2005 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * An implementation of sets over a discrete ordered domain, where the
 * sets are represented by intervals.  It is meant for representing
 * dense sets (e.g., unicode character classes).
 *)

functor IntervalSetFn (D : INTERVAL_DOMAIN) : INTERVAL_SET =
  struct

    structure D = D

    type item = D.point
    type interval = (D.point * D.point)

    fun min (a, b) = (case D.compare(a, b)
	   of LESS => a
	    | _ => b
	  (* end case *))

  (* the set is represented by an ordered list of disjoint, non-adjacent intervals *)
    datatype set = SET of interval list

    val empty = SET[]
    val universe = SET[(D.minPt, D.maxPt)]

    fun isEmpty (SET []) = true
      | isEmpty _ = false

    fun isUniverse (SET[(a, b)]) =
	  (D.compare(a, D.minPt) = EQUAL) andalso (D.compare(b, D.maxPt) = EQUAL)
      | isUniverse _ = false

    fun singleton x = SET[(x, x)]

    fun interval (a, b) = (case D.compare(a, b)
	   of GREATER => raise Domain
	    | _ => SET[(a, b)]
	  (* end case *))

    fun addInterval (SET l, (a, b)) = let
	  fun ins (a, b, []) = [(a, b)]
	    | ins (a, b, (x, y)::r) = (case D.compare(b, x)
		 of LESS => if (D.isSucc(b, x))
		      then (a, y)::r
		      else (a, b)::(x, y)::r
		  | EQUAL => (a, y)::r
		  | GREATER => (case D.compare(a, y)
		       of GREATER => if (D.isSucc(y, a))
			    then (x, b) :: r
			    else (x, y) :: ins(a, b, r)
			| EQUAL => ins(x, b, r)
			| LESS => (case D.compare(b, y)
			     of GREATER => ins (min(a, x), b, r)
			      | _ => ins (min(a, x), y, r)
			    (* end case *))
		      (* end case *))
		(* end case *))
	  in
	    case D.compare(a, b)
	     of GREATER => raise Domain
	      | _ => SET(ins (a, b, l))
	    (* end case *)
	  end
    fun addInterval' (x, m) = addInterval (m, x)

    fun add (SET l, a) = let
	  fun ins (a, []) = [(a, a)]
	    | ins (a, (x, y)::r) = (case D.compare(a, x)
		 of LESS => if (D.isSucc(a, x))
		      then (a, y)::r
		      else (a, a)::(x, y)::r
		  | EQUAL => (a, y)::r
		  | GREATER => (case D.compare(a, y)
		       of GREATER => if (D.isSucc(y, a))
			    then (x, a) :: r
			    else (x, y) :: ins(a, r)
			| _ => (x, y)::r
		      (* end case *))
		(* end case *))
	  in
	    SET(ins (a, l))
	  end
    fun add' (x, m) = add (m, x)

  (* is a point in any of the intervals in the set *)
    fun member (SET l, pt) = let
	  fun look [] = false
	    | look ((a, b) :: r) = (case D.compare(a, pt)
		 of LESS => (case D.compare(pt, b)
			 of GREATER => look r
			  | _ => true
		      (* end case *))
		  | EQUAL => true
		  | GREATER => false
		(* end case *))
	  in
	    look l
	  end

    fun complement (SET[]) = universe
      | complement (SET((a, b)::r)) = let
	  fun comp (start, (a, b)::r, l) =
		comp(D.succ b, r, (start, D.pred a)::l)
	    | comp (start, [], l) = (case D.compare(start, D.maxPt)
		 of LESS => SET(List.rev((start, D.maxPt)::l))
		  | _ => SET(List.rev l)
		(* end case *))
	  in
	    case D.compare(D.minPt, a)
	     of LESS => comp(D.succ b, r, [(D.minPt, D.pred a)])
	      | _ => comp(D.succ b, r, [])
	    (* end case *)
	  end

    fun union (SET l1, SET l2) = let
	  fun join ([], l2) = l2
	    | join (l1, []) = l1
	    | join ((a1, b1)::r1, (a2, b2)::r2) = (case D.compare(a1, a2)
		 of LESS => (case D.compare(b1, b2)
		       of LESS => if D.isSucc(b1, a2)
			    then join(r1, (a1, b2)::r2)
			    else (a1, b1) :: join(r1, (a2, b2)::r2)
			| EQUAL => (a1, b1) :: join(r1, r2)
			| GREATER => join ((a1, b1)::r1, r2)
		      (* end case *))
		  | EQUAL => (case D.compare(b1, b2)
		       of LESS => join(r1, (a2, b2)::r2)
			| EQUAL => (a1, b1) :: join(r1, r2)
			| GREATER => join ((a1, b1)::r1, r2)
		      (* end case *))
		  | GREATER => (case D.compare(a1, b2)
		       of LESS => (case D.compare(b1, b2)
			     of LESS => join (r1, (a2, b2)::r2)
			      | EQUAL => (a2, b2) :: join(r1, r2)
			      | GREATER => join ((a2, b1)::r1, r2)
			    (* end case *))
			| EQUAL => (* a2 < a1 = b2 <= b1 *)
			    join ((a2, b1)::r1, r2)
			| GREATER => if D.isSucc(b2, a1)
			    then join ((a2, b1)::r1, r2)
			    else (a2, b2) :: join ((a1, b1)::r1, r2)
		      (* end case *))
		(* end case *))
	  in
	    SET(join(l1, l2))
	  end

    fun intersect (SET l1, SET l2) = let
	(* cons a possibly empty interval onto the front of l *)
	  fun cons (a, b, l) = (case D.compare(a, b)
		 of GREATER => l
		  | _ => (a, b) :: l
		(* end case *))
	  fun meet ([], _) = []
	    | meet (_, []) = []
	    | meet ((a1, b1)::r1, (a2, b2)::r2) = (case D.compare(a1, a2)
		 of LESS => (case D.compare(b1, a2)
		       of LESS => (* a1 <= b1 < a2 <= b2 *)
			    meet (r1, (a2, b2)::r2)
			| EQUAL => (* a1 <= b1 = a2 <= b2 *)
			    (b1, b1) :: meet (r1, cons(D.succ b1, b2, r2))
			| GREATER => (case D.compare (b1, b2)
			     of LESS => (* a1 < a2 < b1 < b2 *)
				  (a2, b1) :: meet (r1, cons(D.succ b1, b2, r2))
			      | EQUAL => (* a1 < a2 < b1 = b2 *)
				  (a2, b1) :: meet (r1, r2)
			      | GREATER => (* a1 < a2 < b1 & b2 < b1  *)
				  (a2, b2) :: meet (cons(D.succ b2, b1, r1), r2)
			    (* end case *))
		      (* end case *))
		  | EQUAL => (case D.compare(b1, b2)
		       of LESS => (a1, b1) :: meet (r1, cons(D.succ b1, b2, r2))
			| EQUAL => (a1, b1) :: meet (r1, r2)
			| GREATER => (a1, b2) :: meet ((D.succ b2, b1)::r1, r2)
		      (* end case *))
		  | GREATER => (case D.compare(b2, a1)
		       of LESS => (* a2 <= b2 < a1 <= b1 *)
			    meet ((a1, b1)::r1, r2)
			| EQUAL => (* a2 < b2 = a1 <= b1 *)
			    (b2, b2) :: meet (cons(D.succ b2, b1, r1), r2)
			| GREATER => (case D.compare(b1, b2)
			     of LESS => (* a2 < a1 <= b1 < b2 *)
				  (a1, b1) :: meet (r1, cons(D.succ b1, b2, r2))
			      | EQUAL => (* a2 < a1 <= b1 = b2 *)
				  (a1, b1) :: meet (r1, r2)
			      | GREATER => (* a2 < a1 < b2 < b1 *)
				  (a1, b2) :: meet (cons(D.succ b2, b1, r1), r2)
			    (* end case *))
		      (* end case *))
		(* end case *))
	  in
	    SET(meet(l1, l2))
	  end

  (* FIXME: replace the following with a direct implementation *)
    fun difference (s1, s2) = intersect(s1, complement s2)

    fun list (SET l) = l

    fun app f (SET l) = List.app f l

    fun foldl f init (SET l) = List.foldl f init l

    fun foldr f init (SET l) = List.foldl f init l

    fun filter pred (SET l) = let
	  fun f' ([], l) = SET(List.rev l)
	    | f' (i::r, l) = if pred i
		then f'(r, i::l)
		else f'(r, l)
	  in
	    f' (l, [])
	  end

    fun exists pred (SET l) = List.exists pred l

    fun all pred (SET l) = List.all pred l

    fun compare (SET l1, SET l2) = let
	  fun comp ([], []) = EQUAL
	    | comp ((a1, b1)::r1, (a2, b2)::r2) = (case D.compare(a1, a2)
		 of EQUAL => (case D.compare(b1, b2)
		       of EQUAL => comp (r1, r2)
			| someOrder => someOrder
		      (* end case *))
		  | someOrder => someOrder
		(* end case *))
	    | comp ([], _) = LESS
	    | comp (_, []) = GREATER
	  in
	    comp(l1, l2)
	  end

    fun isSubset (SET l1, SET l2) = let
	(* is the interval [a, b] covered by [x, y]? *)
	  fun isCovered (a, b, x, y) = (case D.compare(a, x)
		 of LESS => false
		  | _ => (case D.compare(y, b)
		       of LESS => false
			| _ => true
		      (* end case *))
		(* end case *))
	  fun test ([], _) = true
	    | test (_, []) = false
	    | test ((a1, b1)::r1, (a2, b2)::r2) =
		if isCovered (a1, b1, a2, b2)
		  then test (r1, (a2, b2)::r2)
		  else (case D.compare(b2, a1)
		     of LESS => test ((a1, b1)::r1, r2)
		      | _ => false
		    (* end case *))
	  in
	    test (l1, l2)
	  end

  end
