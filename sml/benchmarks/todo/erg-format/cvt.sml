(* cvt.sml
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

structure Cvt : CVT =
  struct

      (* Imports: Bits *)

    exception BadLog
    exception ShortString
    exception BadPrecision

    val MAXPOSINT = 0x3fffffff
    val MAXNEGINT = ~1073741824  (* 0x40000000 *)
    val masks = Vector.vector[0,1,3,7,15,31,63]
    val negbits = Vector.vector[MAXNEGINT, 0x20000000, 0x10000000,
      0x08000000, 0x04000000, 0x02000000, 0x01000000
    ]
    val posmask = MAXPOSINT
    val dfltChars = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ@_"
    val XChars = "0123456789ABCDEF"

    fun cvtpow2 (lg,chars) = let
      open Vector

      val << = Bits.lshift
      val >> = Bits.rshift
      val & = Bits.andb
      val ++ = Bits.orb
      infix << >> & ++

      val mask = sub(masks, lg)
      val negbit = sub(negbits, lg)

      fun cv v =
        if v = 0 then ""
        else (cv (v >> lg))^(substring(chars,v & mask,1))
    in
      fn 0 => "0"
       | v =>
         if v > 0 then cv v
         else (cv (((v & posmask)>>lg) ++ negbit))^(substring(chars,v & mask,1))
    end

    val cvtdec = Integer.makestring
    val cvthex = cvtpow2(4,dfltChars)
    val cvtheX = cvtpow2(4,XChars)
    val cvtoct = cvtpow2(3,dfltChars)
    val cvtpow2 = 
      (fn (arg as (lg,_)) => if 0 < lg andalso lg < 7 then cvtpow2 arg else raise BadLog)
        handle Substring => raise ShortString

    fun zeros 0 = ""
      | zeros n = "0" ^ zeros(n - 1)

    fun mkdigits (f,0) = ([],if f < 5.0 then 0 else 1)
      | mkdigits (f,i) =
        let 
          val d = floor f
          val new = 10.0 * (f - real d)
          val (digits,carry) = mkdigits (new, i - 1)
          val (digit,c) = 
            case (d,carry) of
              (9,1) => (0,1)
            | _ => (d + carry,0)
        in  
          (digit::digits,c)
        end

      (* decompose:
       * decompose a non-zero real into a list of maxPrec 
       * significant digits, the first non-zero, and
       * integer exponent. The return value
       *  (a::b::c..., exp) is produced from real argument
       *  a.bc... * (10 ^^ exp)
       * If the list would consist of all 9's, the list
       * consisting of 1 followed by all 0's is returned instead.
       *)
    val maxPrec = 15
    fun decompose (f, e, p) =
      if f >= 10.0 then decompose (f/10.0, e+1,p)
      else if f < 1.0 then decompose (f*10.0, e-1,p)
      else let 
        val (digits,carry) = mkdigits(f,max(0,min(p e,maxPrec)))
      in
        case carry of
          0 => (digits,e)
        | _ => (1::digits, e+1)
      end

    fun gcvt (f, prec) = let
      val _ = if prec < 1 then raise BadPrecision else ()
      fun pf _ = prec
        
      fun doFract [] = ""
        | doFract (0::tl) = let 
            val rest = doFract tl
          in case rest of 
            "" => ""
          | _ => "0"^rest
          end
        | doFract (hd::tl) = (cvtdec hd) ^ (doFract tl)

      fun rtoa (sign, (d,e)) = let
        fun doWhole ([], e,wh) =
              if e >= 0 then doWhole([], e-1,"0"::wh)
              else {sign=sign, whole=implode(rev wh), fract="", exp=NONE}
          | doWhole (arg as (hd::tl), e,wh) =
              if e >= 0 then doWhole(tl,e-1,(cvtdec hd)::wh)
              else {sign=sign, whole=implode(rev wh), fract=doFract arg, exp=NONE}
      in
        if e < ~4 orelse e >= prec then 
          {sign=sign, whole=cvtdec (hd d), fract=doFract (tl d), exp=SOME e}
        else if e >= 0 then doWhole(d,e,[])
        else {sign=sign, whole="0", fract=(zeros (~e-1))^(doFract d), exp=NONE}
      end
    in
      if f < 0.0 then rtoa(true,decompose(~f,0,pf))
      else if f > 0.0 then rtoa(false,decompose(f,0,pf))
      else {sign=false,whole="0",fract="",exp=NONE}
    end (* gcvt *)

    fun ecvt (f, prec) = let
      val _ = if prec < 0 then raise BadPrecision else ()
      fun pf _ = prec + 1
        
      fun doFract (_, 0)  = ""
        | doFract ([], n) = "0"^doFract([],n-1)
        | doFract (hd::tl, n) = (cvtdec hd) ^ doFract (tl, n-1)

      fun rtoa (sign, (d, e)) = 
        if prec = 0 then {sign=sign, mantissa=cvtdec (hd d), exp=e}
        else {sign=sign, mantissa=(cvtdec (hd d)) ^ "." ^ (doFract(tl d,prec)), exp=e}
    in
      if f < 0.0 then rtoa(true,decompose(~f,0,pf))
      else if f > 0.0 then rtoa(false,decompose(f,0,pf))
      else if prec = 0 then {sign=false,mantissa="0",exp=0}
      else {sign=false,mantissa="0." ^ (zeros prec), exp=0}
    end (* ecvt *)

    fun fcvt (f, prec) = let
      val _ = if prec < 0 then raise BadPrecision else ()
      fun pf e = e + prec + 1

      fun doFract (_, 0) = ""
        | doFract ([], p) = "0" ^ doFract([], p-1)
        | doFract (hd::tl, p) = (cvtdec hd) ^ doFract(tl, p-1)

      fun doWhole ([], e) =
            if e >= 0 then "0" ^ doWhole(nil,e-1)
            else if prec = 0 then ""
            else "." ^ doFract(nil, prec)
        | doWhole (arg as (hd::tl), e) =
            if e >= 0 then (cvtdec hd) ^ doWhole(tl,e-1)
            else if prec = 0 then ""
            else "." ^ doFract(arg, prec)

      fun rtoa (d, e) = let
        fun doZeros (n, 0) = ""
          | doZeros (1, p) = doFract(d,p)
          | doZeros (n, p) = "0" ^ doZeros(n-1,p-1)
      in
        if e >= 0 then doWhole(d,e)
        else if prec = 0 then "0"
        else "0." ^ doZeros (~e, prec)
      end
    in
      if f < 0.0 then {sign=true,mantissa=rtoa(decompose(~f,0,pf))}
      else if f > 0.0 then {sign=false,mantissa=rtoa(decompose(f,0,pf))}
      else if prec = 0 then {sign=false,mantissa="0"}
      else {sign=false,mantissa="0." ^ (zeros prec)}
    end (* fcvt *)

    fun cvtfreal arg = let
      val {sign,mantissa} = fcvt arg
    in
      if sign then "~" ^ mantissa else mantissa
    end

    fun cvtereal arg = let
      val {sign,mantissa,exp} = ecvt arg
      val s = mantissa ^ "E" ^ (cvtdec exp)
    in
      if sign then ("~" ^ s) else s
    end

  end (* Cvt *)

