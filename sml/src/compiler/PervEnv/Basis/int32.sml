(* int32.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure Int32 : INTEGER =
  struct
    structure I32 = InlineT.Int32

    type int = int32

    val precision = SOME 32

    val minIntVal : int = ~2147483648
    val minInt : int option = SOME minIntVal
    val maxInt : int option = SOME 2147483647

    val op *    : int * int -> int  = I32.*
    val op quot : int * int -> int  = I32.quot
    val op +    : int * int -> int  = I32.+
    val op -    : int * int -> int  = I32.-
    val ~       : int -> int = I32.~
    val op <    : int * int -> bool = I32.<
    val op <=   : int * int -> bool = I32.<=
    val op >    : int * int -> bool = I32.>
    val op >=   : int * int -> bool = I32.>=
    val op =    : int * int -> bool = I32.=
    val op <>   : int * int -> bool = I32.<>

  (* min, max, abs, rem, div, and mod should be inlined. 
   *     ... but this is not the time!
   *)
    fun min(a:int, b:int):int = if I32.<(a,b) then a else b
    fun max(a:int, b:int):int = if I32.>(a,b) then a else b
    fun op rem(a:int,b:int):int =  I32.-(a, I32.*(b, I32.quot(a, b)))
    fun abs(a:int):int = if I32.<(a, 0) then I32.~(a) else a

    fun op div(a:int, b:int):int = if I32.>=(b, 0)
	  then if I32.>=(a, 0)
	    then I32.quot(a, b) 
	    else I32.-(I32.quot(I32.+(a, 1), b), 1)
	  else if I32.>(a,0)
	    then I32.-(I32.quot(I32.-(a, 1), b), 1)
	    else I32.quot(a, b)

    fun op mod(a:int, b:int):int = if I32.>=(b, 0)
	  then if I32.>=(a, 0)
	    then I32.-(a, I32.*(I32.quot(a, b), b))
	    else I32.+(I32.-(a, I32.*(I32.quot(I32.+(a,1), b), b)), b)
	  else if I32.>(a, 0)
	    then I32.+(I32.-(a, I32.*(I32.quot(I32.-(a,1), b), b)), b)
	  else if I32.=(a, ~2147483648) andalso I32.=(b, ~1)
	    then 0
	    else I32.-(a, I32.*(I32.quot(a, b), b))

    fun sign(0) = 0
      | sign i = if I32.<(i, 0) then ~1 else 1

    fun sameSign(i, j) = I32.andb(I32.xorb(i, j), minIntVal) = 0

    fun compare (i:int, j:int) =
	  if (I32.<(i, j)) then General.LESS
	  else if (I32.>(i, j)) then General.GREATER
	  else General.EQUAL

    val scan = NumScan.scanInt
    val fmt = NumFormat.fmtInt
    val toString = fmt StringCvt.DEC
    val fromString = PreBasis.scanString (scan StringCvt.DEC) 

    val toInt : int -> Int.int = I32.toInt
    val fromInt : Int.int -> int = I32.fromInt
    val toLarge : int -> LargeInt.int = I32.toLarge
    val fromLarge : LargeInt.int -> int = I32.fromLarge
  end


(*
 * $Log$
 *)
