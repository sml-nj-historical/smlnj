(* literal-to-num.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
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
    val int64  : IntInf.int -> Word32.word * Word32.word
    val word   : IntInf.int -> word
    val word8  : IntInf.int -> word
    val word32 : IntInf.int -> Word32.word
    val word64 : IntInf.int -> Word32.word * Word32.word
    val isNegative : IntInf.int -> bool
    val repDigits : IntInf.int -> word list  (* expose representation *)
    val lowVal : IntInf.int -> int option
  end

structure LiteralToNum : LITERAL_TO_NUM = struct

    val two_8     : IntInf.int =               0x100
    val two_31    : IntInf.int =          0x80000000
    val two_32    : IntInf.int =         0x100000000
    val two_64    : IntInf.int = 0x10000000000000000
    val int64_min : IntInf.int = ~0x8000000000000000
    val int64_max : IntInf.int =  0x7fffffffffffffff

    fun twowords i =
	(InlineT.IntInf.trunc_word32 (i div two_32),
	 InlineT.IntInf.trunc_word32 i)

    fun negtwowords (x, y) =
	let val (x', y') = (Word32.notb x, Word32.notb y)
	    val y'' = y' + 0w1
	    val x'' = if y'' = 0w0 then x' + 0w1 else x'
	in (x'', y'')
	end

    val int    = Int.fromLarge
    val int32  = Int32.fromLarge
    fun int64 i =
	if i < int64_min orelse i > int64_max then raise Overflow
	else if i < 0 then negtwowords (twowords (~i))
	else twowords i

    fun word8 i =
	if i < 0 orelse i >= two_8 then raise Overflow
	else Word.fromLargeWord
		 (Word8.toLargeWord (InlineT.IntInf.trunc_word8 i))
    fun word i =
	if i < 0 orelse i >= two_31 then raise Overflow
	else InlineT.IntInf.trunc_word31 i
    fun word32 i =
	if i < 0 orelse i >= two_32 then raise Overflow
	else InlineT.IntInf.trunc_word32 i
    fun word64 i =
	if i < 0 orelse i >= two_64 then raise Overflow
	else twowords i

    local
	fun unBI (CoreIntInf.BI x) = x
    in
    val isNegative = #negative o unBI o CoreIntInf.concrete
    val repDigits = #digits o unBI o CoreIntInf.concrete
    fun lowVal i = let
	val l = CoreIntInf.lowValue i
    in
	if l = CoreIntInf.neg_base_as_int then NONE else SOME l
    end
    end
end
