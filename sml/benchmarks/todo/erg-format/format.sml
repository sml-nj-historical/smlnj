(* Copyright 1992 by AT&T Bell Laboratories *)

(* Various formatting functions.
 * Provides comparable capabilities to the
 * ANSI C library printf, with similar syntax.
 * Author: Emden Gansner (erg@ulysses.att.com)
 *)

signature FORMAT = sig

  exception BadFormat
  exception ShortList
  exception BadItem

  datatype plus_style_t = Plus | Blank

    (* Format options :
     *  Alternate form, Left adj, Zero pad, Plus style, 
     *  Min. field width, Precision
     *)
  datatype options_t = OPT of {
    altForm : bool,
    leftAdj : bool,
    zeroPad : bool,
    plus : plus_style_t option,
    minWid : int,
    prec : int option
  }

  datatype fitem_t = 
    Int of int 
  | Str of string 
  | Bool of bool 
  | Re of real 
  | Fmt of (options_t -> string)

  (* Type-safe printf emulator. String argument is a format string,
   * In addition to standard printf semantics, it supports
   *   "b", "B" - bool
   *   "&" - user supplied string-valued function
   * and does not support "u", "n", "p" and "c".
   * format returns the generated string.
   * formatf calls the user-supplied function, passing it
   * generated substrings.
   *
   * Exceptions:
   *   BadFormat - Unexpected format character.
   *   ShortList - More format characters than list items.
   *   BadItem   - Type clash between format character and item.
   *)
  val format : string -> fitem_t list -> string
  val formatf : string -> ((string -> 'a) * fitem_t list) -> unit

end (* FORMAT *)

structure Format : FORMAT = struct

    (* Imports: CType, ByteArray, Cvt *)
    
  datatype plus_style_t = Plus | Blank

  datatype options_t = OPT of {
    altForm : bool,
    leftAdj : bool,
    zeroPad : bool,
    plus : plus_style_t option,
    minWid : int,
    prec : int option
  }
  val dfltOpts = {
    altForm=false, 
    leftAdj=false, 
    zeroPad=false, 
    plus=NONE, 
    minWid=1, 
    prec=NONE
  }

  datatype fitem_t = 
    Int of int 
  | Str of string 
  | Bool of bool 
  | Re of real 
  | Fmt of (options_t -> string)

  exception BadFormat
  exception ShortList
  exception BadItem

    (* pad:
     * Pad string with padlen characters.
     * If leftAdj, pad on right instead of left.
     * If zeroPad, use "0" instead of " ".
     *)
  fun pad (s, padlen, leftAdj, zeroPad) = let
    open ByteArray
    val padding = 
      extract(array(padlen,if zeroPad then ord "0" else ord " "),0,padlen)
  in
    if leftAdj then s^padding else padding^s
  end

    (* findChar:
     * Find character c in string str, starting at 
     * jth character.
     * Return NONE is not found, or SOME i, where
     * i is the index of the c in str.
     *)
  fun findChar c (str,j) = let
    val ord_c = ord c
    fun fc i =
      if ordof(str,i) = ord_c then SOME i
      else fc (i+1)
  in
    (fc j) handle Ord => NONE
  end

  val op sub = fn (s, i) => substring (s, i, 1)
  infix 3 sub

  datatype comp_fmt_t =
    STR of options_t
  | INT8 of options_t
  | INT10 of options_t
  | INT16 of (options_t * bool)
  | BOOL of (options_t * bool)
  | REALF of options_t
  | REALE of (options_t * bool)
  | REALG of (options_t * bool)
  | FMT of options_t
  | CHARS of string
  | STAR of (fitem_t list) -> (comp_fmt_t * fitem_t list)
  (* | RADIX of options_t *)
  
    (* boolString:
     * Convert boolean to string, using options_t.
     *)
  fun boolString (b, doCap, OPT {leftAdj,minWid,prec,altForm,...}) = let
    val bstr = 
      case (doCap, altForm) of
        (false, false) => if b then "true" else "false"
      | (false, true) => if b then "yes" else "no"
      | (true, false) => if b then "TRUE" else "FALSE"
      | (true, true) => if b then "1" else "0"
    val pbstr = 
      case prec of NONE => bstr
                 | SOME p => substring(bstr,0,min(p,size bstr))
    val padlen = minWid - (size pbstr)
  in
    if padlen <= 0 then pbstr
    else pad (pbstr, padlen, leftAdj, false)
  end  

    (* strString:
     * Convert string to string, using options_t.
     *)
  fun strString (s, OPT {leftAdj,minWid,prec,...}) = let
    val str = 
      case prec of NONE => s
                 | SOME p => substring(s,0,min(p,size s))
    val padlen = minWid - (size str)
  in
    if padlen <= 0 then str
    else pad (str, padlen, leftAdj, false)
  end  

  fun greal bigE (r,prec,altForm) = let
    val prec = if prec = 0 then 1 else prec
    val {sign, whole, fract, exp} = Cvt.gcvt (r,prec)
    val man =
      if altForm then 
        let
          val diff = prec - ((size whole) + (size fract))
          val fract' = if diff > 0 then pad(fract,diff,true,true) else fract
        in
          whole ^ "." ^ fract'
        end
      else if fract = "" then whole
      else whole ^ "." ^ fract
    val exp =
      case exp of
        NONE => ""
      | SOME exp => (if bigE then "E" else "e") ^ (Cvt.cvtdec exp)
  in
    (sign, man ^ exp)
  end

  fun ereal bigE (r,prec,altForm) = let
    val {sign, mantissa, exp} = Cvt.ecvt (r,prec)
    val man =
      if altForm andalso prec = 0 then mantissa ^ "." else mantissa
  in
    (sign, man ^ (if bigE then "E" else "e") ^ (Cvt.cvtdec exp))
  end

  fun freal (r,prec,altForm) = let
    val {sign, mantissa} = Cvt.fcvt (r,prec)
  in
    if altForm andalso prec = 0 then (sign, mantissa ^ ".") else (sign, mantissa)
  end

    (* realString:
     * Convert real to string, using options_t.
     * mkstr specifies the function to use for the 
     * conversion.
     *)
  val dfltPrecision = 6
  fun realString (v, mkstr, OPT{leftAdj,zeroPad,minWid,prec,plus,altForm}) = let
    val (sign, rstr) = 
      mkstr(v,case prec of NONE => dfltPrecision | SOME p => p, altForm)
    val prefix =
      if sign then "~"
      else (case plus of NONE => "" | SOME Blank => " " | SOME Plus => "+")

    fun addWid (s,prefixAdj) = let
      val padlen = minWid - (size s) - prefixAdj
    in
      if padlen <= 0 then s
      else pad (s, padlen, leftAdj, zeroPad)
    end
  in
    if size prefix = 0 then addWid(rstr,0)
    else if zeroPad then prefix^(addWid(rstr,size prefix))
    else addWid(prefix^rstr,0)
  end

    (* iString:
     * Common formatter for integers for adding precision and padding.
     * prefix is a prefix string, e.g., "+", "~", "0x", etc.
     * istr is simple string representation of the integer.
     * If precision is specified, zeroPad is ignored.
     *)
  fun iString (prefix, istr, OPT {leftAdj,zeroPad,minWid,prec,...}) = let
    val zeroPad = if prec = NONE then zeroPad else false
    fun addPrec (s, NONE) = s
      | addPrec (s, SOME p) = 
          let
            val padlen = p - (size s)
          in
            if padlen <= 0 then s
            else pad (s, padlen, false, true)
          end

    fun addWid (s,prefixAdj) = let
      val padlen = minWid - (size s) - prefixAdj
    in
      if padlen <= 0 then s
      else pad (s, padlen, leftAdj, zeroPad)
    end
  in
    if size prefix = 0 then addWid(addPrec(istr,prec),0)
    else if zeroPad then prefix^(addWid(addPrec(istr,prec),size prefix))
    else addWid(prefix^(addPrec(istr,prec)),0)
  end

    (* intString:
     * Convert signed int to string, using options_t.
     *)
  fun intString (i, opt as OPT {plus,...}) =
    iString(
      if i < 0 
        then ("~", (Cvt.cvtdec (~i)) handle Overflow => "1073741824", opt)
        else (
          case plus of 
            NONE => "" 
          | SOME Blank => " " 
          | SOME Plus => "+", Cvt.cvtdec i, opt
        )
    )

    (* hexString:
     * Convert int, treated as unsigned, to hex string, using options_t.
     * If cap is true, use "ABCDEF" for hex digits.
     * Alternate form prepends "0x".
     *)
  fun hexString (i, opt as OPT {altForm,...},cap) =
    iString(
      if altForm then "0x" 
      else "", if cap then Cvt.cvtheX i else Cvt.cvthex i, opt)

    (* octString:
     * Convert int, treated as unsigned, to octal string, using options_t.
     * Alternate form increases precision if necessary to guarantee initial "0".
     *)
  fun octString (0, opt) = iString("","0",opt)
    | octString (i, opt as OPT {altForm=false,...}) = 
        iString("",Cvt.cvtoct i,opt)
    | octString (i, OPT{leftAdj,zeroPad,minWid,prec,...}) =
      let
        val ostr = Cvt.cvtoct i
        val p = 
          case prec of 
            NONE => SOME((size ostr)+1) 
          | SOME p => SOME(max(p,(size ostr)+1))
        val opt = 
          OPT{leftAdj=leftAdj,zeroPad=zeroPad,
            minWid=minWid,prec=p,altForm=true,plus=NONE}
      in
        iString("",ostr,opt)
      end

    (* compFormat:
     * Compile format string into a list of strings and
     * (type * options_t) specifications.
     * Syntax of conversion specifications:
     *  %[flags]*[minFieldWidth][.precision]<format>
     *  where
     *   - flags    [- +0#]
     *   - format   [dioXxfeEsb&]
     *   - minFieldWidth, precision  nonnegative int or *
     *)
  fun compFormat fmt = let
    open CType

    fun mkP p = if p < 0 then NONE else SOME p 
    val ord_0 = ord "0"

      (* getFlags:
       * Gather format flags.
       * Enforce:
       *  "-" has precedence over "0"
       *  "+" has precedence over " "
       *)
    fun getFlags i = let
      fun gf(arg as (i,alt,left,zero,plus)) =
        case fmt sub i of
          "-" => gf(i+1,alt,true,false,plus)
        | "+" => gf(i+1,alt,left,zero,SOME Plus)
        | " " => if plus = NONE then gf(i+1,alt,left,zero,SOME Blank)
                 else gf(i+1,alt,left,zero,plus)
        | "#" => gf(i+1,true,left,zero,plus)
        | "0" => if left then gf(i+1,alt,left,false,plus)
                 else gf(i+1,alt,left,true,plus)
        | _ => arg
    in
      gf(i,#altForm dfltOpts,#leftAdj dfltOpts,#zeroPad dfltOpts,#plus dfltOpts)
    end

    fun getnum (i, v) =
      if isDigit(fmt,i) then getnum (i+1, v*10 + ordof(fmt,i) - ord_0) 
      else (i,SOME v)

    fun getMinWid i = let
      val c = fmt sub i
    in
      if c = "*" then (i+1,NONE)
      else if isDigit (c,0) then getnum(i+1,(ord c) - ord_0)
      else (i,SOME(#minWid dfltOpts))
    end

    fun getPrec i =
      if (fmt sub i) = "." then
        let
          val c = fmt sub (i+1)
        in
          if c = "*" then (i+2,NONE)
          else if isDigit (c,0) 
            then (fn (i,v) => (i,SOME v)) (getnum(i+2,(ord c) - ord_0))
          else (i+1,SOME (SOME 0))
        end
      else (i,SOME (#prec dfltOpts))

    fun doFormat i = let
      fun doFmt j =
        case findChar "%" (fmt,j) of
          NONE => 
            (case (size fmt) - i of
              0 => [] 
            | len => [CHARS(substring(fmt,i,len))]
            )
        | SOME k => 
            if (fmt sub (k+1)) = "%" then doFmt(k+2)
            else if k = i then doPercent (k+1)
            else (CHARS(substring(fmt,i,k-i)))::(doPercent (k+1))
    in
      doFmt i
    end
    and doPercent i = let
      val (i',altForm, leftAdj,zeroPad,plus) = getFlags i
      val (i'', minWid) = getMinWid i'
      val (i''', minPrec) = getPrec i''

      fun wrapStar f = let
        val f' = 
          (case minPrec of
            SOME p =>
             (fn (w,l) => 
               f(OPT{altForm=altForm,leftAdj=l,zeroPad=zeroPad,
                 plus=plus,minWid=w,prec=p}))
          | NONE =>
             (fn (w,l) => 
               STAR(fn 
                 ((Int p)::r) => 
                   (f(OPT{altForm=altForm,leftAdj=l,zeroPad=zeroPad,
                     plus=plus,minWid=w,prec=mkP p}),r)
               | _ => raise BadItem
               )
             )
          )
      in
        case minWid of
          SOME w => f' (w,leftAdj)
        | NONE =>
            STAR(fn 
              ((Int w)::r) => 
                (if w < 0 then f'(~w,true) else f'(w,leftAdj),r)
            | _ => raise BadItem
            )
      end
          
      val item = 
        case fmt sub i''' of
          "d" => (fn opt => INT10 opt)
        | "i" => (fn opt => INT10 opt)
        | "o" => (fn opt => INT8 opt)
        | "X" => (fn opt => INT16 (opt,true))
        | "x" => (fn opt => INT16 (opt,false))
        | "f" => (fn opt => REALF opt)
        | "e" => (fn opt => REALE (opt,false))
        | "E" => (fn opt => REALE (opt,true))
        | "g" => (fn opt => REALG (opt,false))
        | "G" => (fn opt => REALG (opt,true))
        | "s" => (fn opt => STR opt)
        | "b" => (fn opt => BOOL (opt,false))
        | "B" => (fn opt => BOOL (opt,true))
        | "&" => (fn opt => FMT opt)
        | _ => raise BadFormat
    in
      (wrapStar item)::(doFormat (i'''+1))
    end
  in
    (doFormat 0) handle _ => raise BadFormat
  end  (* compFormat *)

  fun format fmt = let
    val cfmt = compFormat fmt
    fun f ([],_,acc) = rev acc
      | f ((CHARS s)::rest,vl,acc) = f(rest,vl,s::acc)
      | f ((STAR g)::rest,vl,acc) =
          let
            val (ft,l) = g vl
          in
            f(ft::rest,l,acc)
          end
      | f ((INT10 opt)::rest, (Int i)::rest',acc) = 
          f(rest,rest',(intString(i,opt))::acc)
      | f ((INT8 opt)::rest, (Int i)::rest',acc) = 
          f(rest,rest',(octString(i,opt))::acc)
      | f ((INT16 (opt,cap))::rest, (Int i)::rest',acc) = 
          f(rest,rest',(hexString(i,opt,cap))::acc)
      | f ((REALE (opt,bigE))::rest, (Re r)::rest',acc) = 
          f(rest,rest',(realString(r,ereal bigE,opt))::acc)
      | f ((REALF opt)::rest, (Re r)::rest',acc) = 
          f(rest,rest',(realString(r,freal,opt))::acc)
      | f ((REALG (opt,bigE))::rest, (Re r)::rest',acc) = 
          f(rest,rest',(realString(r,greal bigE,opt))::acc)
      | f ((STR opt)::rest, (Str s)::rest',acc) = 
          f(rest,rest',(strString(s,opt))::acc)
      | f ((BOOL (opt,cap))::rest, (Bool b)::rest',acc) = 
          f(rest,rest',(boolString(b,cap,opt))::acc)
      | f ((FMT opt)::rest, (Fmt g)::rest',acc) = 
          f(rest,rest',(g opt)::acc)
      | f (_,[],_) = raise ShortList 
      | f (_,_::_,_) = raise BadItem
  in
    fn vl => implode(f(cfmt,vl,[]))
  end (* format *)

  fun formatf fmt = let
    val cfmt = compFormat fmt
    fun ff (outf, vl) = let
      fun f ([],_) = ()
        | f ((CHARS s)::rest,vl) = (outf s;f(rest,vl))
        | f ((STAR g)::rest,vl) =
            let
              val (ft,l) = g vl
            in
              f(ft::rest,l)
            end
        | f ((INT10 opt)::rest, (Int i)::rest') = 
            (outf (intString(i,opt));f(rest,rest'))
        | f ((INT8 opt)::rest, (Int i)::rest') = 
            (outf (octString(i,opt));f(rest,rest'))
        | f ((INT16 (opt,cap))::rest, (Int i)::rest') = 
            (outf (hexString(i,opt,cap));f(rest,rest'))
        | f ((REALE (opt,bigE))::rest, (Re r)::rest') = 
            (outf (realString(r,ereal bigE,opt));f(rest,rest'))
        | f ((REALF opt)::rest, (Re r)::rest') = 
            (outf (realString(r,freal,opt));f(rest,rest'))
        | f ((REALG (opt,bigE))::rest, (Re r)::rest') = 
            (outf (realString(r,greal bigE,opt));f(rest,rest'))
        | f ((STR opt)::rest, (Str s)::rest') = 
            (outf (strString(s,opt));f(rest,rest'))
        | f ((BOOL (opt,cap))::rest, (Bool b)::rest') = 
            (outf (boolString(b,cap,opt));f(rest,rest'))
        | f ((FMT opt)::rest, (Fmt g)::rest') = 
            (outf (g opt);f(rest,rest'))
        | f (_,[]) = raise ShortList 
        | f (_,_::_) = raise BadItem
    in
      f (cfmt,vl)
    end
  in
    ff
  end (* formatf *)

end (* Format *)
