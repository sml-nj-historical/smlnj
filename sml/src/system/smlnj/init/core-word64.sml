structure CoreWord64 = struct

  local
      fun notyet () = raise Assembly.Overflow

      infix o val op o = InLine.compose
      val not = InLine.inlnot

      fun lift1' f = f o InLine.w64p
      fun lift1 f = InLine.p64w o lift1' f
      fun lift2' f (x, y) = f (InLine.w64p x, InLine.w64p y)
      fun lift2 f = InLine.p64w o lift2' f

      fun neg64 (hi, 0w0) = (InLine.w32neg hi, 0w0)
	| neg64 (hi, lo) = (InLine.w32notb hi, InLine.w32neg lo)
	      
      fun add64 ((hi1, lo1), (hi2, lo2)) =
	  let val lo = InLine.w32add (lo1, lo2)
	      val hi = InLine.w32add (hi1, hi2)
	  in (if InLine.w32lt (lo, lo1) then InLine.w32add (hi, 0w1) else hi,
	      lo)
	  end
      fun sub64 ((hi1, lo1), (hi2, lo2)) =
	  let val lo = InLine.w32sub (lo1, lo2)
	      val hi = InLine.w32sub (hi1, hi2)
	  in (if InLine.w32gt (lo, lo1) then InLine.w32sub (hi, 0w1) else hi,
	      lo)
	  end
      fun mul64 _ = notyet ()
      fun div64 _ = notyet ()
      fun mod64 _ = notyet ()

      fun swap (x, y) = (y, x)

      fun lt64 ((hi1, lo1), (hi2, lo2)) =
	  InLine.w32lt (hi1, hi2) orelse
	  (InLine.w32eq (hi1, hi2) andalso InLine.w32lt (lo1, lo2))
      val gt64 = lt64 o swap
      val le64 = not o gt64
      val ge64 = not o lt64
  in
      val extern = InLine.w64p
      val intern = InLine.p64w

      val ~ = lift1 neg64
      val + = lift2 add64
      val - = lift2 sub64
      val * = lift2 mul64
      val div = lift2 div64
      val mod = lift2 mod64
      val < = lift2' lt64
      val <= = lift2' le64
      val > = lift2' gt64
      val >= = lift2' ge64
  end
end
