(* vector-slice.sml
 *
 * COPYRIGHT (c) 2000 Bell Labs, Lucent Technologies.
 *
 * extracted from array-slice.mldoc (v. 1.0; 2000-06-20)
 *)


structure VectorSlice : VECTOR_SLICE =
struct

  structure V = InlineT.PolyVector
  val (op +)  = InlineT.DfltInt.+
  val (op -)  = InlineT.DfltInt.-
  val (op <)  = InlineT.DfltInt.<
  val (op >=) = InlineT.DfltInt.>=
  val sub' = V.sub
  val geu = InlineT.DfltInt.geu

  datatype 'a slice = SL of {base: 'a vector, start: int, stop: int}
  (* invariants:
   *   0<=start<=stop<=length base
   *)

(* -- alternate representation with start and length
  datatype 'a slice = SL of {base: 'a vector, start: int, length: int}
  (* invariants:
   *   0<=start,length; start<length base; start+length <= length base *)
*)

(*  val length : 'a slice -> int *)
  fun length(SL{start,stop,...}) = stop - start
(*  fun length(SL{length,...}) = n *)

(* val sub : 'a slice * int -> 'a *)
(* sub(s,j) valid if 0<=j<stop-start, otherwise raises Subscript *)
  fun sub (SL{base, start, stop}, j) =
      let val j' = start+j
       in if geu(j', stop)  (* checks for j' >= 0 *)
          then raise Core.Subscript
          else sub'(base, j')
      end

(* val slice : 'a Vector.vector * int * int option -> 'a slice *)
  fun slice (base,start,lenOp) =
      let val blen = V.length base
       in if geu(start, blen)  (* checks start >= 0 *)
          then raise Core.Subscript
	  else case lenOp
		 of NONE => SL{base=base,start=start,stop=blen}
		  | SOME n => 
		      if geu(n, blen-start) (* checks n >= 0 *)
		      then raise Core.Subscript
		      else SL{base=base,start=start,stop=start+n}
      end
      
(* val full : 'a Vector.vector -> 'a slice *)
  fun full base = SL{base=base,start=0,stop=V.length base}
(*
      let val blen = V.length base
       in if geu(start, blen)  (* checks start >= 0 *)
          then raise Core.Subscript
	  else case lenOp
		 of NONE => SL{base=base,start=start,stop=blen}
		  | SOME n => 
		      if geu(n, blen-start) (* checks n >= 0 *)
		      then raise Core.Subscript
		      else SL{base=base,start=start,stop=start+n}
      end
*)

(* val subslice : 'a slice * int * int option -> 'a slice *)
  fun subslice (SL{base, start, stop}, i, sz) =
      if geu(i, stop-start)  (* checks start >= 0 *)
      then raise Core.Subscript
      else case sz
	     of NONE => SL{base=base,start=start+i,stop=stop}
	      | SOME n => 
		 if geu(n, stop-start-i) (* checks n >= 0 *)
		     then raise Core.Subscript
		 else SL{base=base,start=start+i,stop=start+i+n}

(* val base : 'a slice -> 'a Vector.vector * int * int *)
  fun base (SL{base,start,stop}) = (base,start,stop-start)

(* val vector : 'a slice -> 'a Vector.vector *)
  fun vector (SL{base,start,stop}) =
      Vector.tabulate(stop-start,fn n => sub'(base,n+start))

(* utility functions *)
  fun checkLen n =
      if InlineT.DfltInt.ltu(Vector.maxLen, n)
	  then raise General.Size
      else ()

  fun rev ([], l) = l
    | rev (x::r, l) = rev (r, x::l)

(* val concat : 'a slice list -> 'a Vector.vector *)
    fun concat [v] = vector v
      | concat vl = let
	  (* get the total length and flatten the list *)
	  fun len ([], n, l) = (checkLen n; (n, rev(l, [])))
	    | len (SL{base,start,stop}::r, n, l) = let
		val n' = stop - start
		fun explode (i, l) = if (i < stop)
		      then explode(i+1, sub'(base, i)::l)
		      else l
		in
		  len (r, n + n', explode(start, l))
		end
	  in
	    case len (vl, 0, [])
	     of (0, _) => Assembly.vector0
	      | (n, l) => Assembly.A.create_v(n, l)
	    (* end case *)
	  end

(* val isEmpty : 'a slice -> bool *)
  fun isEmpty (SL{base,start,stop}) = stop<=start

(* val getItem : 'a slice -> ('a * 'a slice) option *)
  fun getItem (SL{base,start,stop}) =
      if stop<=start then NONE
      else SOME(sub'(base, start), SL{base=base,start=start+1,stop=stop})
			      
(* val appi : (int * 'a -> unit) -> 'a slice -> unit *)
  fun appi f (SL{base,start,stop}) =
      let fun app i = if (i < stop)
	      then (f (i, sub'(base, i)); app(i+1))
	      else ()
       in app start
      end

(* val app  : ('a -> unit) -> 'a slice -> unit *)
  fun app f (SL{base,start,stop}) =
      let fun app i = if (i < stop)
	      then (f (sub'(base, i)); app(i+1))
	      else ()
       in app start
      end

(* val mapi : (int * 'a -> 'b) -> 'a slice -> 'b vector *)
  fun mapi f (SL{base,start,stop}) =
      let val len = stop - start
	  fun mapf (i, l) = if (i < stop)
		then mapf (i+1, f (i, sub'(base, i)) :: l)
		else Assembly.A.create_v(len, rev(l, []))
       in if (len > 0)
	      then mapf (start, [])
	  else Assembly.vector0
      end

(* val map  : ('a -> 'b) -> 'a slice -> 'b vector *)
  fun map f (SL{base,start,stop}) =
      let val len = stop - start
	  fun mapf (i, l) = if (i < stop)
		then mapf (i+1, f (sub'(base, i)) :: l)
		else Assembly.A.create_v(len, rev(l, []))
	  in
	    if (len > 0)
	      then mapf (start, [])
	      else Assembly.vector0
	  end

(* val foldli : (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b *)
  fun foldli f init (SL{base,start,stop}) = 
      let fun fold (i, accum) = if (i < stop)
	      then fold (i+1, f (i, sub'(base, i), accum))
	      else accum
       in fold (start, init)
      end

(* val foldri : (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b *)
  fun foldri f init (SL{base,start,stop}) =
      let fun fold (i, accum) = if (i >= start)
	      then fold (i-1, f (i, sub'(base, i), accum))
	      else accum
       in fold (stop - 1, init)
      end

(* val foldl : ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b *)
  fun foldl f init (SL{base,start,stop}) = 
      let fun fold (i, accum) = if (i < stop)
	      then fold (i+1, f (sub'(base, i), accum))
	      else accum
       in fold (start, init)
      end

(* val foldr : ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b *)
  fun foldr f init (SL{base,start,stop}) =
      let fun fold (i, accum) = if (i >= start)
	      then fold (i-1, f (sub'(base, i), accum))
	      else accum
       in fold (stop - 1, init)
      end

(* val findi : (int * 'a -> bool) -> 'a slice -> (int * 'a) option *)
  fun findi f (SL{base,start,stop}) =
      let fun findi' i =
	      if (i < stop)
	      then let val item = (i,sub'(base, i))
		    in if f(item)
		       then SOME item
		       else findi' (i+1)
		   end
	      else NONE
       in findi' start
      end

(* val find  : ('a -> bool) -> 'a slice -> 'a option *)
  fun find f (SL{base,start,stop}) =
      let fun find' i =
	      if (i < stop)
	      then let val item = sub'(base, i)
		    in if f item
		       then SOME(item)
		       else find' (i+1)
		   end
	      else NONE
       in find' start
      end

(* val exists : ('a -> bool) -> 'a slice -> bool *)
  fun exists f (SL{base,start,stop}) =
      let fun exists' i =
	      if (i < stop)
	      then if f(sub'(base, i))
		   then true
		   else exists' (i+1)
	      else false
       in exists' start
      end

(* val all : ('a -> bool) -> 'a slice -> bool *)
  fun all f (SL{base,start,stop}) =
      let fun all' i =
	      if (i < stop)
	      then if f(sub'(base, i))
		   then all' (i+1)
		   else false
	      else true
       in all' start
      end

(* val collate : ('a * 'a -> order) -> 'a slice * 'a slice -> order *)
  fun collate comp (SL{base,start,stop},SL{base=base',start=start',stop=stop'}) =
      let fun cmp (i,i') =
	      if (i < stop)
	      then if (i' >= stop') then GREATER
		   else case comp(sub'(base, i),
		                  sub'(base', i'))
                          of EQUAL => cmp(i+1,i'+1)
			   | x => x
	      else if (i' < stop') then LESS
	      else EQUAL
       in cmp(start,start')
      end
      
(* val copyVec : {src : 'a VectorSlice.slice, dst : 'a Array.array, di : int}
                    -> unit *)
(* DBM: this operation does not involve array slices, so belongs here in
 * VectorSlice, or as John suggests, in Array.
  fun copyVec {src,dst,di} =
      if di < 0 orelse 
	 di + length src > V.length dst 
      then raise Core.Subscript
      else appi(fn (i,x) => V.update(dst,di+i,x)) src
 *)

end (* structure VectorSlice *)
