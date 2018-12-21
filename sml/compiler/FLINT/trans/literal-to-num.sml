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

    val int64  : IntInf.int -> IntInf.int * IntInf.int

    val word64 : IntInf.int -> IntInf.int * IntInf.int

    val isNegative : IntInf.int -> bool

    val repDigits : IntInf.int -> word list  (* expose representation *)

    val lowVal : IntInf.int -> int option
  end

structure LiteralToNum : LITERAL_TO_NUM =
  struct

(* 64BIT: will go away once FLINT knows about 64-bit ints *)
    fun int64 i =
	  (IntInf.andb(IntInf.~>>(i, 0w32), 0xffffffff), IntInf.andb(i, 0xffffffff))

(* 64BIT: will go away once FLINT knows about 64-bit words *)
    fun word64 i = (IntInf.~>>(i, 0w32), IntInf.andb(i, 0xffffffff))

    fun isNegative (i : IntInf.int) = (i < 0)

    local
	fun unBI (CoreIntInf.BI x) = x
    in
    val repDigits = #digits o unBI o CoreIntInf.concrete
    fun lowVal i = let
	  val l = CoreIntInf.lowValue i
	  in
	    if l = CoreIntInf.neg_base_as_int then NONE else SOME l
	  end
    end (* local *)

  end
