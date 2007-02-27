(* staged-allocation-fn.sml
 *
 * 
 *)

functor StagedAllocationFn (
	structure T : MLTREE
	structure TargetLang : TARGET_LANG
) :> STAGED_ALLOCATION where TL = TargetLang = struct

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

  datatype location = 
	   REG of reg                               (* machine register *)
	 | BLOCK_OFFSET of int         (* slot in the overflow block *)
	 | COMBINE of (location * location)         (* a location that occupies two other locations*)
	 | NARROW of (location * width * location_kind) (* a location that loses bits *)  

  type location_info = (width * location * location_kind)

  datatype stage =
	   OVERFLOW of { counter : counter,
			 blockDirection : block_direction,
			 maxAlign : int }
	 | WIDEN of width -> width
	 | CHOICE of ( (slot -> bool) * stage) list
	 | REGS_BY_ARGS of (counter * reg list)
	 | REGS_BY_BITS of (counter * reg list)
	 | BITCOUNTER of counter
	 | ARGCOUNTER of counter
	 | SEQ of stage list
	 | PAD of counter
	 | ALIGN_TO of (width -> width)

  type stepper_fn = (str * slot) -> (str * location_info list)

  val memSize = 8

  local 
    val counter = ref 0
  in
  fun freshCounter () = 
      let val c = !counter
      in
	  counter := c + 1;
	  c
      end (* freshCounter *)
  fun resetCounter () = counter := 0
  end (* local *)

  fun regWidth (w, _) = w

  fun useRegs rs =
      let val c = freshCounter ()
      in
	  (c, SEQ [BITCOUNTER c, REGS_BY_BITS (c, rs)])
      end (* useRegs *)

  fun divides (x, y) = Int.mod (x, y) = 0
  fun toMemSize sz = sz div memSize

  fun insStr (str, c, n) = Str.insert (str, c, n)
  fun findStr (str, c) = valOf (Str.find (str, c))
  val find = findStr

  fun dropBits (0, rs) = rs
    | dropBits (n, []) = []
    | dropBits (n, r :: rs) = 
      let val w = regWidth r
      in
	  if n >= w then dropBits (n - w, rs) else rs
      end (* dropBits *)

  fun drop (0, rs) = rs
    | drop (n, []) = []
    | drop (n, r :: rs) = drop (n - 1, rs)

  fun roundUp (w1, w) = Int.max (w1, w)

  fun init cs = foldl (fn (c, str) => insStr (str, c, 0)) Str.empty cs

  fun allocate ([], str, ls) _ = (str, ls)
    | allocate (OVERFLOW {counter, blockDirection=UP, maxAlign} :: ss, str, ls) 
	       (w, k, al) 
      =
      if divides (maxAlign, al) andalso divides (w, memSize)
      then
	  let val n = findStr (str, counter)
	  in 
	      (insStr (str, counter, n + toMemSize w), (w, BLOCK_OFFSET n, k)  :: ls)
	  end
      else raise StagedAlloc
    | allocate (OVERFLOW {counter, blockDirection=DOWN, maxAlign} :: ss, str, ls) 
	       (w, k, al) 
      =
      if divides (maxAlign, al) andalso divides (w, memSize)
      then
	  let val n = findStr (str, counter)
	      val n' = n + toMemSize w
	  in 
	      (insStr (str, counter, n'), (w, BLOCK_OFFSET (~n'), k) :: ls)
	  end
      else raise StagedAlloc
    | allocate (WIDEN f :: ss, str, ls) (w, k, al) =
      if w <= (f w) then	
	  let val (str', (_, l, _) :: ls') = allocate (ss, str, ls) (f w, k, al)
	      val l' = NARROW (l, w, k)
	  in 
	      (str', (w, l', k) :: ls)
	  end
      else allocate (ss, str, ls) (f w, k, al)
    | allocate (CHOICE cs :: ss, str, ls) (w, k, al) =
      let fun choose [] = raise StagedAlloc
	    | choose ((p, c) :: cs) =
	      if (p (w, k, al)) then c else choose cs
	  val c = choose cs
      in
	  allocate (c :: ss, str, ls) (w, k, al)
      end 
    | allocate (REGS_BY_ARGS (c, rs) :: ss, str, ls) (w, k, al) =
      let val n = findStr (str, c)
	  val rs' = drop (n, rs)
      in
	  (case rs'
	    of [] => allocate (ss, str, ls) (w, k, al)
	     | r :: _ => if (regWidth r) = w 
		       then (str, (w, REG r, k) :: ls) 
		       else raise StagedAlloc
	  (* esac *))
      end
    | allocate (REGS_BY_BITS (c, rs) :: ss, str, ls) (w, k, al) =
      let val n = findStr (str, c)
	  val rs' = dropBits (n, rs)
      in
	  (case rs'
	    of [] => (* insufficient bits *) 
	       allocate (ss, str, ls) (w, k, al)
	     | r :: _ => 
	       if ((regWidth r) = w)
	       then (* the arg fits into the regs *) 
		   (str, (w, REG r, k) :: ls)
	       else (* some of the arg's bits fit into the regs *)
		   let val lWidth = regWidth r
		       val str' = insStr (str, c, n + lWidth)
		       val l = REG r
		       val (str', (_, l', _) :: ls) = 
			   allocate (REGS_BY_BITS (c, rs) :: ss, str', ls) 
				    (w - lWidth, k, al)
		       val l'' = COMBINE (l, l')
		       val n' = findStr (str', c)
		   in
		       (insStr (str', c, n' - lWidth), (w, l'', k) :: ls)
		   end
	  (* esac *))
      end
    | allocate (SEQ ss' :: ss, str, ls) (w, k, al) =
      allocate (ss' @ ss, str, ls) (w, k, al)
    | allocate (BITCOUNTER c :: ss, str, ls) (w, k, al) =
      let val (str', ls') = allocate (ss, str, ls) (w, k, al)
	  val n = findStr (str', c)
      in
	  (insStr (str', c, n + w), ls')
      end
    | allocate (PAD c :: ss, str, ls) (w, k, al) =
      let val n = findStr (str, c)
	  val n' = roundUp (n, al * memSize)
      in 
	  (insStr (str, c, n'), ls)
      end
    | allocate (ALIGN_TO f :: ss, str, ls) (w, k, al) =
      allocate (ss, str, ls) (w, k, f w)
    | allocate (ARGCOUNTER c :: ss, str, ls) (w, k, al) =
      let val (str', ls') = allocate (ss, str, ls) (w, k, al)
	  val n = findStr (str', c)
      in
	  (insStr (str, c, n + 1), ls')
      end (* allocate *)

  fun mkStep stages (str, slot) =
      let val (str, ls) = allocate (stages, str, []) slot
      in
	  (str, rev ls)
      end (* mkStep *)
		  
  fun process {counters, stages} slots =
      let val str0 = init counters
	  val step = mkStep stages
	  fun processSlot (slot, (str, lss)) =
	      let val (str, ls) = step (str, slot)
	      in
		  (str, ls :: lss)
	      end (* processSlot *)
	  val (_, lss) = foldl processSlot (str0, []) slots
      in
	  resetCounter ();
	  rev lss
      end (* process *)

end (* StagedAllocationFn *)
