(* literal-to-num.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Conversions from integer/word literals (which are represented as
 * arbitrary precision ints) to fixed size.
 *
 * This structure is a hack, which should be replaced by a parameterized
 * numeric types.
 *)

signature LITERAL_TO_NUM =
  sig
    val int    : IntInf.int -> int
    val int32  : IntInf.int -> Int32.int
    val word   : IntInf.int -> word
    val word8  : IntInf.int -> word
    val word32 : IntInf.int -> Word32.word
  end

structure LiteralToNum : LITERAL_TO_NUM =
  struct

    val zero = IntInf.fromInt 0
    val one = IntInf.fromInt 1
    val two = IntInf.fromInt 2
    val two_8 = IntInf.pow(two, 8)
    val two_30 = IntInf.pow(two, 30)
    val two_31 = IntInf.pow(two, 31)
    val two_32 = IntInf.pow(two, 32)
    val neg_two_30 = IntInf.~ two_31
    val neg_two_31 = IntInf.~ two_31

  (* return n if it is in the range [lo..hi-1]; otherwise raise Overflow *)
    fun chkIntRange (lo, hi) n =
	  if (IntInf.<=(lo, n) andalso IntInf.<(n, hi))
	    then n
	    else raise Overflow

  (* return n if it is in the range [0..hi-1]; otherwise raise Overflow *)
    fun chkWordRange hi n = if (IntInf.<(n, hi)) then n else raise Overflow

    fun int i = IntInf.toInt(chkIntRange (neg_two_30, two_30) i)
    fun int32 i = IntInf.toLarge(chkIntRange (neg_two_31, two_31) i)
    fun word w = Word.fromLargeInt(IntInf.toLarge(chkWordRange two_31 w))
    fun word8 w = Word.fromInt(IntInf.toInt(chkWordRange two_8 w))
    fun word32 w =
	  if (IntInf.>=(w, two_32))
	    then raise Overflow
	    else Word32.fromLargeInt(IntInf.toLarge w)
	      handle Overflow => let
		val (d, m) = IntInf.divmod(w, two)
		val d = Word32.fromLargeInt(IntInf.toLarge d)
		in
		  if (m = zero) then Word32.<<(d, 0w1) else Word32.<<(d, 0w1)+0w1
		end

  end


(*
 * $Log: literal-to-num.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:16  george
 * Version 110.5
 *
 *)
