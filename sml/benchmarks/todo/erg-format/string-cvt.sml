(*
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.
 *
 * Various functions for converting strings to
 * integer and real values.
 *
 * AUTHOR:  Emden Gansner
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    erg@ulysses.att.com
 *)

structure StringCvt : STRING_CVT = struct
    
  exception BadBase
  exception BadIntRep
  exception BadRealRep

  open CType

  (* skipWS:
   * remove leading blanks.
   *)
  fun skipWS (s,i) = let
    fun skip j = if isSpace (s,j) then skip (j+1) else j
  in
    skip i
  end

  val ord_neg = ord "~"
  fun isNegative (s,i) = if ordof(s,i) = ord_neg then (true,i+1) else (false,i)

  val ord_x = ord "x"
  val ord_X = ord "X"
  val lower_base = ord "a" - 10
  val upper_base = ord "A" - 10
  val ord_0 = ord "0"
  val ord_9 = ord "9"
  val maxBase = 10 + (ord "z" - ord "a") + 1

  fun strtoi {s, first, base} = let
    fun digit base i =
      (if isAlphaNum(s,i) then let
        val d = if isDigit(s,i) then ordof(s,i) - ord_0
                else if isLower(s,i) then ordof(s,i) - lower_base
                else ordof(s,i) - upper_base
        in
          if d < base then SOME d else NONE
        end
      else NONE) handle Ord => NONE

    fun getBase (i,0) = let
          val b = ordof(s,i)
          in
            if b = ord_0 then let 
              val x = ordof(s,i+1) 
              in 
                if x = ord_x orelse x = ord_X then (i+2,16) else (i,8)
              end handle Ord => (i,8)
            else (i,10)
          end
      | getBase (b as (i,16)) = 
          ((if ordof(s,i) = ord_0 andalso 
             isXDigit(s,i+2) andalso
             let val x = ordof(s,i+1) in x = ord_x orelse x = ord_X end
          then (i+2,16) else b) handle Ord => b)
      | getBase (b as (_,base)) = 
          if base < 0 orelse base > maxBase then raise BadBase else b

    fun convert (i, base) = let
      val digit = digit base
      fun loop (arg as (v,i)) = 
        case digit i of
          NONE => arg
        | SOME d => loop(base*v-d,i+1)
      in
        case digit i of
          NONE => raise BadIntRep
        | SOME v => loop(~v,i+1)
      end

    val (negative, i) = isNegative(s,skipWS(s,first))
    val (v, ii) = convert (getBase(i,base))
    in
      { v = if negative then v else ~v, next = ii}
    end handle Ord => raise BadIntRep

  fun atoi s = #v(strtoi{s=s,base=10,first=0})
  fun xatoi s = #v(strtoi{s=s,base=16,first=0})
  fun oatoi s = #v(strtoi{s=s,base=8,first=0})

  (* strtor:
   *
   * Converts decimal representation of real into real.
   * From SML/NJ compiler
   *)
  fun strtor {s,first} = let 
    datatype digit = Dot | Exp | Minus | Digit of int | Unknown

    fun mkdigit i =
      (case ordof (s,i) of
        46 (* ord "." *) => Dot
      | 69 (* ord "E" *) => Exp
      | 126 (* ord "~" *) => Minus
      | d => if d >= ord_0 andalso d <= ord_9 then Digit(d - ord_0) 
             else Unknown
      ) handle Ord => Unknown

    val (negative, index) = isNegative(s,skipWS(s,first))

    fun mkReal(exp,d::dl,mant) = mkReal(exp,dl,mant * 0.1 + real(d))
      | mkReal(0,[],mant) = if negative then ~mant else mant
      | mkReal(exp,[],mant) = 
          if exp>0 then mkReal(exp-1,[],mant*10.0) 
          else mkReal(exp+1,[],mant*0.1)

    fun doExp (i,r,idx) = let
      val {v=exp, next} = 
        (strtoi{s=s,first=idx,base=10}) handle _ => {v=0,next=idx-1}
      in
        {v=mkReal(exp+i,r,0.0),next=next}
      end

    fun doFract(i,f,r,idx) =
      case mkdigit idx of
        Exp => if i+f > 0 then doExp(i-1,r,idx+1) else raise BadRealRep
      | Digit d => doFract(i,f+1,d::r,idx+1)
      | _ => if i+f > 0 then {v=mkReal(i-1,r,0.0),next=idx} else raise BadRealRep

    fun doWhole(i,r,idx)=
      case mkdigit idx of
        Dot => if i = 0 then doFract(0,0,[0],idx+1) else doFract(i,0,r,idx+1)
      | Exp => if i = 0 then raise BadRealRep else doExp(i-1,r,idx+1)
      | Digit d => doWhole(i+1,d::r,idx+1)
      | _ => if i > 0 then {v=mkReal(i-1,r,0.0),next=idx} else raise BadRealRep

  in 
    doWhole(0,[],index)
  end handle Ord => raise BadRealRep

  fun atof s = #v(strtor{s=s,first=0})

end (* StringCvt *)
