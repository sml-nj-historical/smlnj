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
      infix o val op o = InLine.compose
      val not = InLine.inlnot
      infix 7 * val op * = InLine.w32mul
      infix 6 + - val op + = InLine.w32add val op - = InLine.w32sub
      infix 5 << >> val op << = InLine.w32lshift val op >> = InLine.w32rshiftl
      infix 5 & val op & = InLine.w32andb
      infix 4 < val op < = InLine.w32lt

      fun lift1' f = f o InLine.i64p
      fun lift1 f = InLine.p64i o lift1' f
      fun lift2' f (x, y) = f (InLine.i64p x, InLine.i64p y)
      fun lift2 f = InLine.p64i o lift2' f

      fun neg64 (0wx80000000, 0w0) = raise Assembly.Overflow
	| neg64 (hi, 0w0) = (InLine.w32neg hi, 0w0)
	| neg64 (hi, lo) = (InLine.w32notb hi, InLine.w32neg lo)

      fun notyet _ = raise Assembly.Overflow

      val add64 = notyet
      val sub64 = notyet
      val mul64 = notyet
      val div64 = notyet
      val mod64 = notyet

      fun negbit hi = InLine.w32ne (InLine.w32andb (hi, 0wx80000000), 0w0)

      fun swap (x, y) = (y, x)

      fun lt64 ((hi1, lo1), (hi2, lo2)) =
	  let fun normal () =
		  hi1 < hi2 orelse (InLine.w32eq (hi1, hi2) andalso lo1 < lo2)
	  in if negbit hi1 then
		 if negbit hi2 then normal ()
		 else true
	     else normal ()
	  end
      val gt64 = lt64 o swap
      val le64 = not o gt64
      val ge64 = not o lt64

      fun abs64 (hi, lo) = if negbit hi then neg64 (hi, lo) else (hi, lo)
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
      val > = lift2' gt64
      val >= = lift2' ge64
      val abs = lift1 abs64
  end
end
