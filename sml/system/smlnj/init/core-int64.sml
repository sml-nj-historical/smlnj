(* core-int64.sml
 *
 *   Basic (simulated) 64-bit integer support.
 *
 * Copyright (c) 2004 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure CoreInt64 = struct

  local
      structure CII = CoreIntInf

      infix o       val op o = InLine.compose
      infix 7 *     val op * = InLine.w32mul
      infix 6 + -   val op + = InLine.w32add     val op - = InLine.w32sub
      infix 5 << >> val op << = InLine.w32lshift val op >> = InLine.w32rshiftl
      infix 5 &     val op & = InLine.w32andb
      infix 4 <     val op < = InLine.w32lt
      infix 4 >     val op > = InLine.w32gt
      infix 4 <>    val op <> = InLine.w32ne
      infix 4 ==    val op == = InLine.w32eq
      val not = InLine.inlnot

      fun lift1' f = f o InLine.i64p
      fun lift1 f = InLine.p64i o lift1' f
      fun lift2' f (x, y) = f (InLine.i64p x, InLine.i64p y)
      fun lift2 f = InLine.p64i o lift2' f

      fun neg64 (0wx80000000, 0w0) = raise Assembly.Overflow
	| neg64 (hi, 0w0) = (InLine.w32neg hi, 0w0)
	| neg64 (hi, lo) = (InLine.w32notb hi, InLine.w32neg lo)

      fun negbit hi = hi & 0wx80000000
      fun isneg hi = negbit hi <> 0w0

      fun add64 ((hi1, lo1), (hi2, lo2)) =
	  let val (hi, lo) = (hi1 + hi2, lo1 + lo2)
	      val hi = if lo < lo1 then hi + 0w1 else hi
	      val nb1 = negbit hi1
	  in if nb1 <> negbit hi2 orelse nb1 == negbit hi then (hi, lo)
	     else raise Assembly.Overflow
	  end

      fun sub64 ((hi1, lo1), (hi2, lo2)) =
	  let val (hi, lo) = (hi1 - hi2, lo1 - lo2)
	      val hi = if lo1 < lo then hi - 0w1 else hi
	      val nb1 = negbit hi1
	  in if nb1 == negbit hi2 orelse nb1 == negbit hi then (hi, lo)
	     else raise Assembly.Overflow
	  end

      (* I am definitely too lazy to do this the pedestrian way, so
       * here we go... *)
      fun mul64 (x, y) =
	  CII.testInf64 (CII.* (CII.extendInf64 x, CII.extendInf64 y))

      fun div64 (_, (0w0, 0w0)) = raise Assembly.Div
	| div64 (x, (0w0, 0w1)) = x
	| div64 (x, (0wxffffffff, 0wxffffffff)) = neg64 x
	| div64 (x, y) =
	    (* again, the easy way out... *)
	    CII.truncInf64 (CII.div (CII.extendInf64 x, CII.extendInf64 y))

      fun mod64 (x, y) = sub64 (x, mul64 (div64 (x, y), y))

      fun lt64 ((hi1, lo1), (hi2, lo2)) = (case (isneg hi1, isneg hi2)
	     of (true, false) => true
	      | (false, true) => false
	      | _ => hi1 < hi2 orelse (hi1 == hi2 andalso lo1 < lo2)
	    (* end case *))
      fun gt64 ((hi1, lo1), (hi2, lo2)) = (case (isneg hi1, isneg hi2)
	     of (true, false) => false
	      | (false, true) => true
	      | _ => hi1 > hi2 orelse (hi1 == hi2 andalso lo1 > lo2)
	    (* end case *))
      val le64 = not o gt64
      val ge64 = not o lt64

      fun abs64 (hi, lo) = if isneg hi then neg64 (hi, lo) else (hi, lo)
  in
      val extern = InLine.i64p
      val intern = InLine.p64i

      val ~ = lift1 neg64
      val op + = lift2 add64
      val op - = lift2 sub64
      val op * = lift2 mul64
      val div = lift2 div64
      val mod = lift2 mod64
      val op < = lift2' lt64
      val <= = lift2' le64
      val op > = lift2' gt64
      val >= = lift2' ge64
      val abs = lift1 abs64
  end
end
