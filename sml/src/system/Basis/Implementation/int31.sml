(* int31.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * The following structures must be without signatures so that inlining 
 * can take place: Bits, Vector, Array, RealArray, Int, Real
 *
 *)

structure Int31Imp : INTEGER =
  struct
    structure I31 = InlineT.Int31
    structure I32 = InlineT.Int32

    exception Div = Assembly.Div
    exception Overflow = Assembly.Overflow

    type int = int

    val precision = SOME 31
    val minIntVal = ~1073741824
    val minInt = SOME minIntVal
    val maxInt = SOME 1073741823

    val toLarge : int -> LargeInt.int = I31.toLarge
    val fromLarge : LargeInt.int -> int = I31.fromLarge
    val toInt = I31.toInt
    val fromInt = I31.fromInt

    val ~ 	: int -> int = I31.~
    val op * 	: int * int -> int  = I31.*
    val op + 	: int * int -> int  = I31.+
    val op - 	: int * int -> int  = I31.-
    val op div 	: int * int -> int  = I31.div
    val op mod 	: int * int -> int  = I31.mod
    val op quot : int * int -> int  = I31.quot
    val op rem 	: int * int -> int  = I31.rem
    val min 	: int * int -> int  = I31.min
    val max 	: int * int -> int  = I31.max
    val abs 	: int -> int = I31.abs

    fun sign 0 = 0
      | sign i = if I31.<(i, 0) then ~1 else 1
    fun sameSign (i, j) = (I31.andb(I31.xorb(i, j), minIntVal) = 0)

    fun compare (i, j) =
	  if (I31.<(i, j)) then General.LESS
	  else if (I31.>(i, j)) then General.GREATER
	  else General.EQUAL
    val op > 	: int * int -> bool = I31.>
    val op >= 	: int * int -> bool = I31.>=
    val op < 	: int * int -> bool = I31.<
    val op <= 	: int * int -> bool = I31.<=

    fun fmt radix = (NumFormat.fmtInt radix) o Int32Imp.fromInt

    fun scan radix = let
      val scanLarge = NumScan.scanInt radix
      fun f getc cs = 
	(case scanLarge getc cs
	  of NONE => NONE
	   | SOME(i, cs') => 
	     if I32.>(i, 0x3fffffff) orelse I32.<(i, ~0x40000000) then
	       raise Overflow
	     else
	       SOME(Int32Imp.toInt i, cs')
	(*esac*))
    in f
    end

    val toString = fmt StringCvt.DEC
(*
    val fromString = PreBasis.scanString (scan StringCvt.DEC)
*)
    local
	structure W31 = InlineT.Word31
	structure CV = InlineT.CharVector
    in
    (* optimized version of fromString; it is about 2x as fast as
     * using scanString: *)
    fun fromString s =
	let val n = size s
	    val z = ord #"0"
	    val sub = CV.sub
	    infix ++
	    fun x ++ y = W31.toIntX (W31.+ (W31.fromInt x, W31.fromInt y))
	    fun num (i, a) =
		if i >= n then a
		else let val c = ord (sub (s, i)) - z
		     in
			 if c < 0 orelse c > 9 then a
			 else num (i ++ 1, 10 * a + c)
		     end
	    fun nonneg i =
		if i >= n then NONE
		else let val c = ord (sub (s, i)) - z
		     in
			 if c < 0 orelse c > 9 then NONE
			 else SOME (num (i ++ 1, c))
		     end
	    fun skipwhite i =
		if i >= n then NONE
		else let val c = sub (s, i)
		     in
			 if Char.isSpace c then skipwhite (i ++ 1)
			 else if c = #"-" orelse c = #"~" then
			     Option.map ~ (nonneg (i ++ 1))
			 else nonneg i
		     end
	in
	    skipwhite 0
	end
    end (* local *)
  end  (* structure Int31 *)

