(* time.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure TimeImp : TIME =
  struct

    structure PB = PreBasis
    structure LInt = LargeIntImp
    structure Real = RealImp
    structure Int = IntImp
    structure Int32 = Int32Imp
    structure String = StringImp

  (* get time type from type-only structure *)
    open Time

    exception Time

    val zeroTime = PB.TIME{seconds=0, uSeconds=0}

    fun toSeconds (PB.TIME{seconds, ...}) = seconds
    fun fromSeconds sec =
	  if (sec < 0)
	    then raise Time
	    else PB.TIME{seconds=sec, uSeconds=0}

    fun toMilliseconds (PB.TIME{seconds, uSeconds}) =
	  (seconds * 1000) + LInt.quot(uSeconds, 1000)
    fun fromMilliseconds msec =
	  if (msec < 0)
	    then raise Time
	  else if (msec >= 1000)
	    then PB.TIME{seconds= LInt.quot(msec, 1000),
			 uSeconds= 1000*(LInt.rem(msec, 1000))}
	    else PB.TIME{seconds= 0, uSeconds= 1000*msec}

    fun toMicroseconds (PB.TIME{seconds, uSeconds}) =
	  (seconds * 1000000) + uSeconds
    fun fromMicroseconds usec =
	  if (usec < 0)
	    then raise Time
	  else if (usec >= 1000000)
	    then PB.TIME{seconds= LInt.quot(usec, 1000000),
			 uSeconds= LInt.rem(usec,  1000000)}
	    else PB.TIME{seconds=0, uSeconds=usec}

    local
    (* a floor function that produces a LargeInt.int *)
      fun floor x = Real.toLargeInt IEEEReal.TO_NEGINF x
    in
    fun fromReal rt = if (rt < 0.0)
	  then raise Time
	  else let
	    val sec = floor rt
	    in
	      PB.TIME{seconds=sec,
		      uSeconds=floor((rt - Real.fromLargeInt sec) * 1000000.0)}
	    end
	      handle Overflow => raise Time
    end (* local *)

    fun toReal (PB.TIME{seconds, uSeconds}) =
	Real.fromLargeInt seconds + 0.000001 * Real.fromLargeInt uSeconds

    fun add (PB.TIME{seconds=s1, uSeconds=u1},
	     PB.TIME{seconds=s2, uSeconds=u2}) = let
	  val s = s1 + s2
	  val u = u1+u2
	  in
	    if (u >= 1000000)
	      then PB.TIME{seconds=s+1, uSeconds=u-1000000}
	      else PB.TIME{seconds=s, uSeconds=u}
	  end
    fun sub (PB.TIME{seconds=s1, uSeconds=u1},
	     PB.TIME{seconds=s2, uSeconds=u2}) = let
	  val s = s1 - s2
	  val u = u1 - u2
	  val (s, u) = if (u < 0) then (s-1, u+1000000) else (s, u)
	  in
	    if (s < 0)
	      then raise Time
	      else PB.TIME{seconds=s, uSeconds=u}
	  end

    fun compare (PB.TIME{seconds=s1, uSeconds=u1},
		 PB.TIME{seconds=s2, uSeconds=u2}) =
	  if (s1 < s2) then LESS
	  else if (s1 = s2)
	    then if (u1 < u2) then LESS
	    else if (u1 = u2) then EQUAL
	    else GREATER
	  else GREATER

    fun less (PB.TIME{seconds=s1, uSeconds=u1},
	      PB.TIME{seconds=s2, uSeconds=u2}) =
	  (s1 < s2) orelse ((s1 = s2) andalso (u1 < u2))
    fun lessEq (PB.TIME{seconds=s1, uSeconds=u1},
		PB.TIME{seconds=s2, uSeconds=u2}) =
	  (s1 < s2) orelse ((s1 = s2) andalso (u1 <= u2))

    local
      val gettimeofday : unit -> (Int32.int * int) =
	    CInterface.c_function "SMLNJ-Time" "timeofday"
    in
    fun now () = let val (ts, tu) = gettimeofday()
	  in
	    PB.TIME{seconds= Int32.toLarge ts, uSeconds= Int.toLarge tu}
	  end
    end (* local *)

    local
      val zeros = "0000000000"
      val numZeros = String.size zeros
      fun pad 0 = []
	| pad n = if (n <= numZeros)
	    then [substring(zeros, 0, n)]
	    else zeros :: pad(n - numZeros)
      val rounding = #[
	      PB.TIME{seconds=0, uSeconds= 50000},
	      PB.TIME{seconds=0, uSeconds=  5000},
	      PB.TIME{seconds=0, uSeconds=   500},
	      PB.TIME{seconds=0, uSeconds=    50},
	      PB.TIME{seconds=0, uSeconds=     5}
	    ]
      val fmtInt = (NumFormat.fmtInt StringCvt.DEC)
      fun fmtUSec usec = let
	    val usec' = fmtInt usec
	    in
	      String.substring(zeros, 0, 6 - String.size usec') ^ usec'
	    end
    in
    fun fmt prec = if (prec <= 0)
	    then let
	      fun fmt' t = let
		    val PB.TIME{seconds, ...} =
			add(t, PB.TIME{seconds=0, uSeconds=500000})
		    in
		      fmtInt seconds
		    end
	      in
		fmt'
	      end
	  else if (prec >= 6)
	    then let
	      fun fmt' (PB.TIME{seconds, uSeconds}) =
		    String.concat(fmtInt seconds :: "." ::
				  fmtUSec uSeconds :: pad(prec-6))
	      in
		fmt'
	      end
	    else let (* 0 < prec < 6 *)
	      val amt = InlineT.PolyVector.sub(rounding, prec-1)
	      fun fmt' t = let
		    val PB.TIME{seconds, uSeconds} = add(t, amt)
		    in
		      String.concat[
			  fmtInt seconds, ".",
			  String.substring(fmtUSec uSeconds, 0, prec)
			]
		    end
	      in
		fmt'
	      end
(*
    fun fmt prec (PB.TIME{sec, usec}) = let
	  val sec' = fmtInt sec
	  in
	    if (prec <= 0)
	      then sec'
	      else let
		val usec' = fmtInt usec
		val frac = String.substring(zeros, 0, 6 - String.size usec') ^ usec'
		in
		  if (prec < 6)
		    then String.concat [
			sec', ".", String.substring(frac, 0, prec)
		      ]
		    else String.concat (sec' :: "." :: frac :: pad(prec-6))
		end
	  end
*)
    end (* local *)

  (* scan a time value; this has the syntax:
   *
   *  [0-9]+(.[0-9]+)? | .[0-9]+
   *)
    fun scan getc charStrm = let
	  val chrLE : (char * char) -> bool = InlineT.cast InlineT.DfltInt.<=
	  fun isDigit c = (chrLE(#"0", c) andalso chrLE(c, #"9"))
	  fun incByDigit (n, c) = 10*n + Int.toLarge(CharImp.ord c - CharImp.ord #"0")
	  fun scanSec (secs, cs) = (case (getc cs)
		 of NONE => SOME(PB.TIME{seconds=secs, uSeconds=0}, cs)
		  | (SOME(#".", cs')) => (case (getc cs')
		       of NONE => SOME(PB.TIME{seconds=secs, uSeconds=0}, cs)
			| (SOME(d, cs'')) => if (isDigit d)
			    then scanUSec (secs, cs')
			    else SOME(PB.TIME{seconds=secs, uSeconds=0}, cs)
		      (* end case *))
		  | (SOME(d, cs')) => if (isDigit d)
		      then scanSec(incByDigit(secs, d), cs')
		      else SOME(PB.TIME{seconds=secs, uSeconds=0}, cs)
		(* end case *))
	  and scanUSec (secs, cs) = let
		fun normalize (usecs, 6) = usecs
		  | normalize (usecs, n) = normalize(10*usecs, n+1)
		fun scan' (usecs, 6, cs) = (case (getc cs)
		       of NONE => (usecs, cs)
			| (SOME(d, cs')) => if (isDigit d)
			    then scan' (usecs, 6, cs')
			    else (usecs, cs)
		      (* end case *))
		  | scan' (usecs, ndigits, cs) = (case (getc cs)
		       of NONE => (normalize(usecs, ndigits), cs)
			| (SOME(d, cs')) => if (isDigit d)
			    then scan' (incByDigit(usecs, d), ndigits+1, cs')
			    else (normalize(usecs, ndigits), cs)
		      (* end case *))
		val (usecs, cs) = scan' (0, 0, cs)
		in
		  SOME(PB.TIME{seconds=secs, uSeconds=usecs}, cs)
		end
	  val cs = PB.skipWS getc charStrm
	  in
	    case (getc cs)
	     of NONE => NONE
	      | (SOME(#".", cs')) => (case (getc cs')
		   of NONE => NONE
		    | (SOME(d, _)) =>
			if (isDigit d) then scanUSec (0, cs') else NONE
		  (* end case *))
	      | (SOME(d, _)) => if (isDigit d) then scanSec(0, cs) else NONE
	    (* end case *)
	  end

    val toString   = fmt 3
    val fromString = PB.scanString scan

    val (op +) = add
    val (op -) = sub

    val (op <)  = less
    val (op <=) = lessEq
    val (op >)  = Bool.not o lessEq
    val (op >=) = Bool.not o less

  end (* TIME *)

