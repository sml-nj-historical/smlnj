(* scan.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.
 *
 * Function to scan input. Similar syntax and semantics to C scanf.
 *
 * AUTHOR:  Emden Gansner
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    erg@ulysses.att.com
 *)

structure Scan : SCAN =
  struct

    structure F = Format

    datatype token =
      Int of int option * bool * int
    | Str of int option * bool
    | Pat of int option * bool * (string *int -> bool)
    | Bool of int option * bool 
    | Re of int option * bool 
    | Ch of int option * bool
    | Literal of string
    | White
    | Count

    exception BadFormat
    exception EOS
    exception Fail

    val ord_percent = ord "%"
    val ord_rbracket = ord "]"
    val ord_caret = ord "^"
    val ord_dash = ord "-"
    val ord_star = ord "*"

    (* Utility functions. *)

    (* skipWS :
     * Given string and string index, return index of next
     * non-white character.
     *)
    fun skipWS (arg as (s, i)) = 
      if CType.isSpace arg then skipWS (s, i+1) else i

    (* match:
     * Match s, starting at i, against tests
     * Return true if the same
     *)
    fun match tests (s, i) = let
      val testlen = size tests
      fun check j =
        if j = testlen then true
        else if ordof(s,i+j) = ordof(tests,j) then check (j+1)
        else false
      in
        (check 0) handle Ord => false
      end

    (* atoa: 
     *)
    fun atoa wid (s, i) = let
      val w = case wid of NONE => 0 | SOME w => w
      fun next j = 
        (if j - i = w then j
        else if CType.isSpace (s,j) then j
        else next (j+1)) handle Ord => j
      val nexti = next (i+1)
      in
        (F.Str (substring (s, i, nexti - i)), nexti)
      end
  
    (* atoc: 
     *)
    fun atoc (SOME w) (s, i) = let
          val nexti = min(i+w,size s)
          in
            if i = nexti then raise Fail
            else (F.Str (substring (s, i, nexti - i)), nexti)
          end
      | atoc _ _ = raise BadFormat
  
    (* atob: 
     *)
    fun atob wid (s, i) = let
      val w = case wid of NONE => ~1 | SOME w => w
      fun match (test, ans) = let
        val tlen = size test
        fun loop j = 
          if j = tlen orelse j = w then (F.Bool ans, j+i)
          else if ordof(test,j) = ord(CType.toLower(substring(s,i+j,1))) 
            then loop (j+1)
          else raise Fail
        in
          loop 0
        end
      in
        (match("true",true)) handle Fail => match("false",false)
      end
  
    (* atof: 
     *)
    fun atof wid (s, i) = let
      val (s',delta) = case wid of 
                         NONE => (s,0)
                       | SOME w => (substring(s,i,min(w,(size s)-1)),i)
      val {v,next} = StringCvt.strtor{s=s',first=i-delta}
      in
        (F.Re v, next+delta)
      end handle _ => raise Fail
  
    (* mkPatScan: 
     *)
    fun mkPatScan (okayfn, wid) =
      fn (s, i) => let
        val maxj = case wid of NONE => size s 
                             | SOME w => min(i+w,size s)
        fun next j =
          if j = maxj then j
          else if okayfn(s,j) then next(j+1)
          else j
        val nexti = next i
        in
          if i = nexti then raise Fail
          else (F.Str (substring (s, i, nexti - i)), nexti)
        end

    (* mkIntScan: 
     *)
    fun mkIntScan (base, wid) =
      fn (s, i) => let
        val (s',delta) = case wid of 
                           NONE => (s,0)
                         | SOME w => (substring(s,i,min(w,(size s)-1)),i)
        val {v,next} = StringCvt.strtoi{s=s',first=i-delta,base=base}
        in
          (F.Int v, next+delta)
        end

    fun scanEOS _ = []

    fun scanFn (f, assign, skipWhite, contFn) = let
      val skip = if skipWhite then skipWS else fn (_,i) => i
      in
        fn (s,i) => let
          val (v, newi) = f (s,skip(s,i))
          in
            if assign then v::(contFn(s,newi)) else contFn(s,newi)
          end handle _ => []
      end

    fun scanWhite contfn = fn (s,i) => (contfn(s,skipWS(s,i))) handle Ord => []

    fun scanLiteral (vs, contfn) = let
      val vslen = size vs
      in
        fn (s, i) => let
          val nexti = skipWS (s,i)
          in
            if match vs (s, nexti) then contfn (s, nexti + vslen)
            else []
          end handle Ord => []
      end

    fun mkScanner s = let

      fun scanSpace i = let
        val nexti = skipWS(s,i)
        in
          (White, nexti)
        end handle Ord => (White, size s)

      fun scanLit i = let
        fun next j =
          (if CType.isSpace (s,j) then j
           else if ordof(s,j) = ord_percent then
             (if ordof(s,j+1) = ord_percent then next(j+2)
             else j) handle Ord => raise BadFormat
           else next (j+1)) handle Ord => j
        val nexti = next (i+1)
        in
          (Literal (substring (s,i,nexti-i)), nexti)
        end

      fun scanAssign i = 
        if ordof(s,i) = ord_star then (false, i+1)
        else (true, i)

      fun scanPat (width,assign,i) = let
        open Array
        val (dflt,set,i') = if ordof(s,i) = ord_caret then (true,false,i+1)
                            else (false, true, i)
        val table = array(256,dflt)
        val i'' = let
          val c = ordof(s,i')
          in
            if c = ord_rbracket orelse c = ord_dash 
              then (update(table,c,set);i'+1)
              else i'
          end

        fun setRange(b,d) = 
          if b > d then true
          else (update(table,b,set);setRange(b+1,d))

        fun isRange j = let
          val d = ordof(s,j+1)
          val b = ordof(s,j-1)
          in
            if d <> ord_rbracket andalso b < d then setRange(b,d)
            else false
          end

        fun loop j = let
          val c = ordof(s,j)
          in
             if c = ord_rbracket then j+1
             else if c = ord_dash andalso isRange j then loop(j+2)
             else (update(table,c,set); loop(j+1))
          end
        val nexti = loop i''
        in
          (Pat(width,assign,fn a => sub(table, ordof a)),nexti)
        end

      fun scanWidth i =
        if CType.isDigit (s,i) then let
          val {v, next} = StringCvt.strtoi{s=s,first=i,base=10}
          in
            if v > 0 then (SOME v, next) else raise BadFormat
          end
        else (NONE, i)

      fun scanFormat i = let
        val (assign,ii) = scanAssign i
        val (width, iii) = scanWidth ii
        in
          case substring(s, iii, 1) of
            "d" => (Int(width, assign, 10), iii+1)
          | "i" => (Int(width, assign, 0), iii+1)
          | "o" => (Int(width, assign, 8), iii+1)
          | "x" => (Int(width, assign, 16), iii+1)
          | "e" => (Re(width, assign), iii+1)
          | "f" => (Re(width, assign), iii+1)
          | "g" => (Re(width, assign), iii+1)
          | "s" => (Str(width, assign), iii+1)
          | "[" => scanPat(width, assign, iii+1)
          | "c" => 
            (case width of
              NONE => (Ch(SOME 1, assign), iii+1)
            |  _ => (Ch(width, assign), iii+1))
          | "b" => (Bool(width, assign), iii+1)
          | "%" => if iii = i then scanLit(i-1) else raise BadFormat
          | "n" => if iii = i then (Count, iii+1) else raise BadFormat
          |  _ => raise BadFormat
        end handle Ord => raise BadFormat

      fun scanToken i =
        (if ordof(s, i) = ord_percent then scanFormat (i+1)
         else if CType.isSpace(s,i) then scanSpace (i+1)
         else scanLit i) handle Ord => raise EOS

      fun mks i = let
        val (token, nexti) = scanToken i
        val continuation = mks nexti
        in
          case token of
            Int (w,b,base) => scanFn (mkIntScan (base,w),b,true,continuation)
          | Str (w,b) => scanFn (atoa w,b,true,continuation)
          | Bool (w,b) => scanFn (atob w,b,true,continuation)
          | Re (w,b) => scanFn (atof w,b,true,continuation)
          | Ch (w,b) => scanFn (atoc w,b,false,continuation)
          | Pat (w,b,pat) => scanFn (mkPatScan(pat,w),b,false,continuation)
          | Literal s => scanLiteral (s, continuation)
          | White => scanWhite continuation
          | Count => fn arg as (_, i) => (F.Int i)::(continuation arg)
        end handle EOS => scanEOS
      in
        mks 0
      end

    fun scanner argfn format = let
          val scanfn = mkScanner format
          in
            fn s => scanfn (argfn s)
          end

    val scan = scanner (fn s => (s,0))
    val scani = scanner (fn s => s)
  end
