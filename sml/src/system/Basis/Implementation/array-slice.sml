(* array-slice.sml
 *
 * COPYRIGHT (c) 2000 Bell Labs, Lucent Technologies.
 *
 * extracted from array-slice.mldoc (v. 1.0; 2000-06-20)
 *)


structure ArraySlice : ARRAY_SLICE =
struct

  val (op +)  = InlineT.DfltInt.+
  val (op -)  = InlineT.DfltInt.-
  val (op <)  = InlineT.DfltInt.<
  val (op >=) = InlineT.DfltInt.>=
  val sub' = InlineT.PolyArray.sub
  val update' = InlineT.PolyArray.update
  val geu = InlineT.DfltInt.geu

  datatype 'a slice = SL of {base: 'a array, start: int, stop: int}
  (* invariants:
   *   0<=start<=stop<=length base
   *)

(* -- alternate representation with start and length
  datatype 'a slice = SL of {base: 'a array, start: int, length: int}
  (* invariants:
   *   0<=start,length; start<length base; start+length <= length base *)
*)

(*  val length : 'a slice -> int *)
  fun length(SL{start,stop,...}) = stop - start
(*  fun length(SL{length,...}) = n *)

(* val sub : 'a slice * int -> 'a *)
(* sub(s,j) valid if 0<=j<stop-start *)
  fun sub (SL{base, start, stop}, j) =
      let val j' = start+j
       in if geu(j', stop)  (* checks for j' >= 0 *)
          then raise Core.Subscript
          else sub'(base, j')
      end

(* val update : 'a slice * int * 'a -> unit *)
  fun update (SL{base, start, stop}, j, x) =
      let val j' = start+j
       in if geu(j', stop)  (* checks for j' >= 0 *)
          then raise Core.Subscript
          else update'(base, j',x)
      end
    
(* val slice : 'a Array.array * int * int option -> 'a slice *)
  fun slice (base,start,lenOp) =
      let val blen = InlineT.PolyArray.length base
       in if geu(start, blen)  (* checks start >= 0 *)
          then raise Core.Subscript
	  else case lenOp
		 of NONE => SL{base=base,start=start,stop=blen}
		  | SOME n => 
		      if geu(n, blen-start) (* checks n >= 0 *)
		      then raise Core.Subscript
		      else SL{base=base,start=start,stop=start+n}
      end
      
(* val full : 'a Array.array -> 'a slice *)
  fun full base = SL{base=base,start=0,stop=InlineT.PolyArray.length base}

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

(* val base : 'a slice -> 'a Array.array * int * int *)
  fun base (SL{base,start,stop}) = (base,start,stop-start)

(* val vector : 'a slice -> 'a vector *)
  fun vector (SL{base,start,stop}) =
      Vector.tabulate((fn n => sub'(base,n+start)),
		      stop-start)

(* val copy    : {src : 'a slice, dst : 'a Array.array, di : int} -> unit *)
  fun copy {src = SL{base,start,stop},dst,di} =
      if di < 0 orelse 
	 di + (stop - start) > InlineT.PolyArray.length dst 
      then raise Core.Subscript
      else if di <= start
      then let fun cp i =
                   if (i < stop)
	           then (update'(dst,di+i,sub'(base, i));
		         cp(i+1))
	           else ()
	    in app start
	   end
      else let fun cp i =
                   if (i >= start)
	           then (update'(dst,di+i,sub'(base, i));
		         cp(i-1))
	           else ()
	    in cp (stop-1)
	   end

(* val copyVec : {src : 'a VectorSlice.slice, dst : 'a Array.array, di : int}
                    -> unit *)
(* DBM: this operation does not involve array slices, so should not be here. *)
  fun copyVec {src,dst,di} =
      if di < 0 orelse 
	 di + VectorSlice.length src > InlineT.PolyArray.length dst 
      then raise Core.Subscript
      else VectorSlice.appi(fn (i,x) => update'(dst,di+i,x)) src

(* val isEmpty : 'a slice -> bool *)
  fun isEmpty (SL{base,start,stop}) = stop<=start

(* val getItem : 'a slice -> ('a * 'a slice) option *)
  fun getItem (SL{base,start,stop}) =
      if stop<=start then NONE
      else SOME(sub'(base, j'), SL{base,start+1,stop})
			      
  end

(* val appi : (int * 'a -> unit) -> 'a slice -> unit *)
  fun appi f (SL{base,start,stop}) =
      let fun app i = if (i < stop)
	      then (f (i, sub'(base, i)); app(i+1))
	      else ()
       in app start
      end

(* val app  : ('a -> unit) -> 'a slice -> unit *)
  fun appi f (SL{base,start,stop} =
      let fun app i = if (i < stop)
	      then (f (sub'(base, i)); app(i+1))
	      else ()
       in app start
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
  fun foldli f init (SL{base,start,stop}) = 
      let fun fold (i, accum) = if (i < stop)
	      then fold (i+1, f (sub'(base, i), accum))
	      else accum
       in fold (start, init)
      end

(* val foldr : ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b *)
  fun foldri f init (SL{base,start,stop}) =
      let fun fold (i, accum) = if (i >= start)
	      then fold (i-1, f (sub'(base, i), accum))
	      else accum
       in fold (stop - 1, init)
      end

(* val modifyi : (int * 'a -> 'a) -> 'a slice -> unit *)
  fun modifyi f (SL{base,start,stop}) =
      let fun modify' i =
	      if (i < stop)
	      then (
		update'(base, i, f (i, sub'(base, i)));
		modify'(i+1))
	      else ()
       in modify' start
      end

(* val modify  : ('a -> 'a) -> 'a slice -> unit *)
  fun modify f (SL{base,start,stop}) =
      let fun modify' i = 
	      if (i < stop)
	      then (update'(base, i, f(sub'(base, i)));
		    modify'(i+1))
	      else ()
       in modify' start
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
		       else findi' (i+1)
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
  fun exists f (SL{base,start,stop}) =
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
		   else case comp(sub'(base, i)),
		                  sub'(base', i'))
                          of EQUAL => cmp(i+1,i'+1)
			   | x => x
	      else if (i' < stop') then LESS
	      else EQUAL
       in cmp(start,start')
      end
      
end (* structure ArraySlice *)
