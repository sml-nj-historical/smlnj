(* cvt-sig.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.
 *
 * Basic value to string conversions.
 *
 * AUTHOR:  Emden Gansner
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    erg@ulysses.att.com
 *)

signature CVT =
  sig
    exception BadLog
    exception ShortString
    exception BadPrecision

      (* Convert signed integer to string *)
    val cvtdec : int -> string

      (* Convert integer, treated as unsigned, into
       * power of 2 radix notation.
       * First argument specifies integer power of 2
       * and string of conversion characters. 
       * Exceptions:
       *   BadLog : power < 1 or power > 6
       *   ShortString : not enough characters in 
       *     conversion string
       *)
    val cvtpow2 : (int * string) -> int -> string

      (* Specific power of 2 conversions.
       * Equivalent to 
       *   cvtheX = cvtpow2(4,"0123456789ABCDEF")
       *   cvthex = cvtpow2(4,"0123456789abcdef")
       *   cvtoct = cvtpow2(3,"01234567")
       *)
    val cvtoct : int -> string
    val cvthex : int -> string
    val cvtheX : int -> string

      (* Low-level conversion of real to string, with given precision.
       * In ecvt and fcvt, precision specifies number of fractional digits
       * with 0's appended if necessary.
       * In gcvt, precision specifies the number of significant digits;
       * however, trailing 0's in the fractional part are dropped.
       * Exceptions:
       *   BadPrecision : precision < 0 for ecvt,fcvt or < 1 for gcvt
       *)
    val ecvt : (real * int) -> {sign:bool, mantissa:string, exp:int}
    val fcvt : (real * int) -> {sign:bool, mantissa:string}
    val gcvt : (real * int) -> 
      {sign:bool, whole:string, fract:string, exp: int option}

      (* Convert real to string, with given precision.
       * Precision specifies number of fractional digits.
       *   cvtereal - [~]d.dddE<sign>dd 
       *   cvtfreal - [~]ddd.ddd
       * Exceptions:
       *   BadPrecision : precision < 0
       *)
    val cvtereal : (real * int) -> string
    val cvtfreal : (real * int) -> string

  end (* CVT *)

