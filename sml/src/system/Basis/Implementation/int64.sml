(* int64.sml
 *
 *   64-bit integers
 *
 * Copyright (c) 2004 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure Int64 : INTEGER = struct

    type int = Int64.int

    val extern = InlineT.Int64.extern
    val intern = InlineT.Int64.intern

    val precision = SOME 64

    val minIntVal : int = ~0x8000000000000000
    val minInt : int option = SOME minIntVal
    val maxInt : int option = SOME 0x7fffffffffffffff

    val toLarge = CoreIntInf.extendInf64 o CoreInt64.extern
    val fromLarge = CoreInt64.intern o CoreIntInf.testInf64

    fun negbit hi = Word32Imp.andb (hi, 0wx80000000)
    fun isneg hi = negbit hi <> 0w0

    fun toInt i =
	let val mask = 0wxc0000000
	in case extern i of
	       (0w0, lo) =>
	         if Word32Imp.andb (lo, mask) = 0w0 then Word32Imp.toInt lo
		 else raise Assembly.Overflow
	     | (0wxffffffff, lo) =>
	         if Word32Imp.andb (lo, mask) = mask then Word32Imp.toIntX lo
		 else raise Assembly.Overflow
	  | _ => raise Assembly.Overflow
	end

    fun fromInt i31 =
	let val i32 = Int32Imp.fromInt i31
	    val hi = if i32 < 0 then 0wxffffffff else 0w0
	in intern (hi, InlineT.Word32.copyf_int32 i32)
	end

    fun quot (x, y) = fromLarge (IntInfImp.quot (toLarge x, toLarge y))
    fun rem (x, y) = x - quot (x, y) * y

    fun sign 0 = 0
      | sign i = if isneg (#1 (extern i)) then ~1 else 1

    fun sameSign (x, y) = sign x = sign y

    fun min (x: int, y) = if x < y then x else y
    fun max (x: int, y) = if x > y then x else y

    fun compare (x, y) =
	let val (hi1, lo1) = extern x
	    val (hi2, lo2) = extern y
	    fun normal () =	(* same-sign case *)
		if hi1 < hi2 then LESS
		else if hi1 > hi2 then GREATER
		else if lo1 < lo2 then LESS
		else if lo1 > lo2 then GREATER
		else EQUAL
	in if isneg hi1 then
	       if isneg hi2 then normal () else LESS
	   else if isneg hi2 then GREATER
	   else normal ()
	end

    fun fmt rdx i = IntInfImp.fmt rdx (toLarge i)
    val toString = fmt StringCvt.DEC

    fun scan rdx rdr s =
	case IntInfImp.scan rdx rdr s of
	    SOME (i, s') =>
	      if i < ~0x80000000 orelse i > 0x7fffffff then
		  raise Assembly.Overflow
	      else SOME (intern (CoreIntInf.truncInf64 i), s')
	  | NONE => NONE

    val fromString = PreBasis.scanString (scan StringCvt.HEX)

    val ~      : int -> int        = ~
    val op +   : int * int -> int  = op +
    val op -   : int * int -> int  = op -
    val op *   : int * int -> int  = op *
    val op div : int * int -> int  = op div
    val op mod : int * int -> int  = op mod
    val abs    : int -> int        = abs
    val op <   : int * int -> bool = op <
    val op <=  : int * int -> bool = op <=
    val op >   : int * int -> bool = op >
    val op >=  : int * int -> bool = op >=
end
