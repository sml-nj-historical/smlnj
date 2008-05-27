(* staged-allocation-fn.sml
 *
 * This code implements the Staged Allocation technique for calling conventions.
 * You can find the POPL06 paper describing this technique at
 * http://www.eecs.harvard.edu/~nr/pubs/staged-abstract.html
 * 
 * Mike Rainey (mrainey@cs.uchicago.edu)
 *
 *)

functor StagedAllocationFn (
    structure T : MLTREE
    structure TargetLang : TARGET_LANG
    (* number of bits addressable in the target machine *)
    val memSize : int
  ) :> STAGED_ALLOCATION 
	   where TL = TargetLang = 
  struct

    structure T = T
    structure TL = TargetLang

    type width = int
    type reg = (width * T.reg)
    type location_kind = TL.location_kind
    type counter = int
    datatype block_direction = UP | DOWN
    type slot = (width * location_kind * int)

    exception StagedAlloc
	      
    structure Str = IntBinaryMap
    type str = int Str.map

    (* locations consist of machine registers, offsets in to overflow blocks, combinations of
     * locations, and narrowed locations.
     *)
    datatype location 
      = REG of reg
      | BLOCK_OFFSET of int
      | COMBINE of (location * location)  
      | NARROW of (location * width * TL.location_kind) 

    type location_info = (width * location * location_kind)

    (* language for specifying calling conventions *)
    datatype stage 
      = OVERFLOW of {                                (* overflow block (usually corresponds to a runtime stack) *)
	     counter : counter,
	     blockDirection : block_direction,
	     maxAlign : int 
        }
      | WIDEN of (width -> width)      
      | CHOICE of ( (slot -> bool) * stage) list     (* choose the first stage whose corresponding 
						      * predicate is true. *)
      | REGS_BY_ARGS of (counter * reg list)         (* the first n arguments go into the first n
						      * registers *)
      | ARGCOUNTER of counter
      | REGS_BY_BITS of (counter * reg list)         (* the first n bits arguments go into the first 
						      * n bits of registers *)
      | BITCOUNTER of counter                        
      | SEQ of stage list                            (* sequence of stages *)
      | PAD of counter                               (* specifies an alignment (this rule applies even 
						      * for registers) *)      
      | ALIGN_TO of (width -> width)                 (* specifies an alignment *)

    type stepper_fn = (str * slot) -> (str * location_info)
					  
  (* global counter for allocating in the store *)
    local 
	val counter = ref 0
    in

    fun freshCounter () = let
	val c = !counter
        in
	  counter := c + 1;
	  c
        end

    fun resetCounter () = counter := 0

    end (* local *)

    fun regWidth (w, _) = w
			  
    fun useRegs rs = let
	val c = freshCounter ()
        in
	    (c, SEQ [BITCOUNTER c, REGS_BY_BITS (c, rs)])
        end

    fun divides (x, y) = Int.mod (x, y) = 0
    fun toMemSize sz = sz div memSize

    fun insStr (str, c, n) = Str.insert (str, c, n)
    fun findStr (str, c) = (case Str.find (str, c)
          of SOME v => v
	   | NONE => raise Fail "staged allocation: cannot find counter in store"
          (* end case *))
    val find = findStr

    fun dropBits (0, rs) = rs
      | dropBits (n, []) = []
      | dropBits (n, r :: rs) = let
        val w = regWidth r
	in
	  if n >= w then dropBits (n - w, rs) else rs
	end

    fun drop (0, rs) = rs
      | drop (n, []) = []
      | drop (n, r :: rs) = drop (n - 1, rs)
			    
    fun roundUp (w1, w) = Int.max (w1, w)
			  
    fun init cs = List.foldl (fn (c, str) => insStr (str, c, 0)) Str.empty cs

    (* this function implements the state transition rules of Figure 6 in the Staged Allocation paper. *)
    fun allocate ([], str, locs) _ = (str, locs)
      (* allocate to the overflow block *)
      | allocate (OVERFLOW {counter, blockDirection=UP, maxAlign} :: ss, str, locs) 
		 (w, k, al) 
	=
	if divides (maxAlign, al) andalso divides (w, memSize)
	then
	    let val n = findStr (str, counter)
	    in 
		(insStr (str, counter, n + toMemSize w), (w, BLOCK_OFFSET n, k) :: locs)
	    end
	else raise StagedAlloc
      (* allocate to the overflow block *)
      | allocate (OVERFLOW {counter, blockDirection=DOWN, maxAlign} :: ss, str, locs) 
		 (w, k, al) 
	=
	if divides (maxAlign, al) andalso divides (w, memSize)
	then
	    let val n = findStr (str, counter)
		val n' = n + toMemSize w
	    in 
		(insStr (str, counter, n'), (w, BLOCK_OFFSET (~n'), k) :: locs)
	    end
	else raise StagedAlloc
      (* widen a location *)
      | allocate (WIDEN f :: ss, str, locs) (w, k, al) =
	if w <= (f w) then	
	    let val (str', (_, l, _) :: _) = allocate (ss, str, locs) (f w, k, al)
		val l' = NARROW (l, w, k)
	    in 
		(str', (w, l', k) :: locs)
	    end
	else allocate (ss, str, locs) (f w, k, al)
      (* choose the first stage whose corresponding predicate is true. *)
      | allocate (CHOICE cs :: ss, str, locs) (w, k, al) =
	let fun choose [] = raise StagedAlloc
	      | choose ((p, c) :: cs) =
		if (p (w, k, al)) then c else choose cs
	    val c = choose cs
	in
	    allocate (c :: ss, str, locs) (w, k, al)
	end 
      (* the first n arguments go into the first n registers *)
      | allocate (REGS_BY_ARGS (c, rs) :: ss, str, locs) (w, k, al) =
	let val n = findStr (str, c)
	    val rs' = drop (n, rs)
	in
	    case rs'
	      of [] => allocate (ss, str, locs) (w, k, al)
	       | r :: _ => if (regWidth r) = w 
			   then (str, (w, REG r, k) :: locs) 
			   else raise StagedAlloc
	end
      | allocate (ARGCOUNTER c :: ss, str, locs) (w, k, al) =
	let val (str', locs') = allocate (ss, str, locs) (w, k, al)
	    val n = findStr (str', c)
	in
	    (insStr (str', c, n + 1), locs')
	end      
      (* the first n bits arguments go into the first n bits of registers *)
      | allocate (REGS_BY_BITS (c, rs) :: ss, str, locs) (w, k, al) =
	let val n = findStr (str, c)
	    val rs' = dropBits (n, rs)
	in
	    case rs'
	     of [] => (* insufficient bits *) 
		allocate (ss, str, locs) (w, k, al)
	      | r :: _ => 
		if ((regWidth r) = w)
		then (* the arg fits into the regs *) 
		    (str, (w, REG r, k) :: locs)
		else (* some of the arg's bits fit into the regs *)
		    let val lWidth = regWidth r
			val str' = insStr (str, c, n + lWidth)
			val l = REG r
			val (str', (_, l', _) :: _) = 
			    allocate (REGS_BY_BITS (c, rs) :: ss, str', locs) 
				     (w - lWidth, k, al)
			val l'' = COMBINE (l, l')
			val n' = findStr (str', c)
		    in
			(insStr (str', c, n' - lWidth), (w, l'', k) :: locs)
		    end
	end
      | allocate (BITCOUNTER c :: ss, str, locs) (w, k, al) =
	let val (str', locs') = allocate (ss, str, locs) (w, k, al)
	    val n = findStr (str', c)
	in
	    (insStr (str', c, n + w), locs')
	end
      (* sequence of stages *)
      | allocate (SEQ ss' :: ss, str, locs) (w, k, al) =
	allocate (ss' @ ss, str, locs) (w, k, al)
      (* specifies an alignment (this rule applies even for registers) *)
      | allocate (PAD c :: ss, str, locs) (w, k, al) =
	let val n = findStr (str, c)
	    val n' = roundUp (n, al * memSize)
	in 
	    (insStr (str, c, n'), locs)
	end
      (* specifies an alignment *)
      | allocate (ALIGN_TO f :: ss, str, locs) (w, k, al) =
	allocate (ss, str, locs) (w, k, f w)
	
  (* staging returns only a single location at present *)
    fun mkStep stages (str, slot) = 
	(case allocate (stages, str, []) slot
	  of (str, [l]) => (str, l)
	   | _ => raise StagedAlloc
	(* end case *))
	
  (* perform staged allocation over a list of slots *)
    fun doStagedAllocation (str, stepper, slots) = let
        fun f (loc, (str, saLocs)) = let
            val (str', saLoc) = stepper(str, loc)
            in
	       (str', saLoc :: saLocs)
            end
        (* staged allocation converts abstract locations to machine locations *)
        val (str', saLocs) = List.foldl f (str, []) slots
        in
	   (str', List.rev saLocs)
        end


end (* StagedAllocationFn *)
