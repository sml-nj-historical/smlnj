(* date.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

local
    structure Int = IntImp
    structure Int32 = Int32Imp
    structure Time = TimeImp
    structure PB = PreBasis
    structure Char = CharImp
in
structure Date : DATE =
  struct


	(* the run-time system indexes the year off this *)
	val baseYear = 1900
	    
	exception Date

	datatype weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun

	datatype month
	    = Jan | Feb | Mar | Apr | May | Jun
	  | Jul | Aug | Sep | Oct | Nov | Dec

	datatype date = DATE of {
				 year : int,
				 month : month,
				 day : int,
				 hour : int,
				 minute : int,
				 second : int,
				 offset : Time.time option,
				 wday : weekday,
				 yday : int,
				 isDst : bool option
				 }

	(* tables for mapping integers to days/months *)
	val dayTbl = #[Sun, Mon, Tue, Wed, Thu, Fri, Sat]
	val monthTbl = #[Jan, Feb, Mar, Apr, May, Jun, Jul, 
			 Aug, Sep, Oct, Nov, Dec]

	fun dayToInt (d) = (case d
				of Sun => 0 | Mon => 1 | Tue => 2 | Wed => 3
			      | Thu => 4 | Fri => 5 | Sat => 6
	(* end case *))

	(* careful about this: the month numbers are 0-11 *)
	fun monthToInt m = (case m
				of Jan => 0 | Feb => 1 | Mar => 2 | Apr => 3 | May => 4 | Jun => 5
			      | Jul => 6 | Aug => 7 | Sep => 8 | Oct => 9 | Nov => 10 | Dec => 11
	(* end case *))

	(* the tuple type used to communicate with C; this 9-tuple has the fields:
	 * tm_sec, tm_min, tm_hour, tm_mday, tm_mon, tm_year, tm_wday, tm_yday,
	 * and tm_isdst.
	 *)
(*	type tm = (int * int * int * int * int * int * int * int * int) *)
        type tm = SMLBasis.Date_t

	(* wrap a C function call with a handler that maps SysErr exception into 
	 * Date exceptions.
	 *)
	fun wrap f x = (f x) handle _ => raise Date

(*
	(* note: mkTime assumes the tm structure passed to it reflects
	 * the local time zone
	 *)
	val ascTime : tm -> string = SMLBasis.ascTime  (* dbm: missing *)
(*	    = wrap (CInterface.c_function "SMLNJ-Date" "ascTime") *)
	val localTime : Int32.int -> tm = SMLBasis.localTime
(*	    = wrap (CInterface.c_function "SMLNJ-Date" "localTime") *)
	val gmTime : Int32.int -> tm = SMLBasis.gmTime
(*	    = wrap (CInterface.c_function "SMLNJ-Date" "gmTime") *)
	val mkTime : tm -> Int32.int = SMLBasis.mkTime  (* dbm: missing *)
(*	    = wrap (CInterface.c_function "SMLNJ-Date" "mkTime") *)
	val strfTime : (string * tm) -> string = SMLBasis.strfTime  (* dbm: missing *)
(*	    = wrap (CInterface.c_function "SMLNJ-Date" "strfTime") *)
*)

	fun year (DATE{year, ...}) = year
	fun month (DATE{month, ...}) = month
	fun day (DATE{day, ...}) = day
	fun hour (DATE{hour, ...}) = hour
	fun minute (DATE{minute, ...}) = minute
	fun second (DATE{second, ...}) = second
	fun weekDay (DATE{wday, ...}) = wday
	fun yearDay (DATE{yday, ...}) = yday
	fun isDst (DATE{isDst, ...}) = isDst
	fun offset (DATE{offset,...}) = offset


	(* 
	 * This code is taken from Reingold's paper
	 *)
	local 
	    val quot = Int.quot
	    val not = Bool.not
	    fun sum (f,k,p) = 
		let fun loop (f,i,p,acc) = if (not(p(i))) then acc
					   else loop(f,i+1,p,acc+f(i))
		in
		    loop (f,k,p,0)
		end
	    fun lastDayOfGregorianMonth (month,year) =
		if ((month=1) andalso 
		    (Int.mod (year,4) = 0) andalso 
		    not (Int.mod (year,400) = 100) andalso
		    not (Int.mod (year,400) = 200) andalso
		    not (Int.mod (year,400) = 300))
		    then 29
		else List.nth ([31,28,31,30,31,30,31,31,30,31,30,31],month)
	in
	    fun toAbsolute (month, day, year) =
		day  
		+ sum (fn (m) => lastDayOfGregorianMonth(m,year),0,
		       fn (m) => (m<month)) 
		+ 365 * (year -1)
		+ quot (year-1,4)
		- quot (year-1,100)
		+ quot (year-1,400)
	    fun fromAbsolute (abs) =
		let val approx = quot (abs,366)
		    val year = (approx + sum(fn(_)=>1, approx, 
					     fn(y)=> (abs >= toAbsolute(0,1,y+1))))
		    val month = (sum (fn(_)=>1, 0,
				      fn(m)=> (abs > toAbsolute(m,lastDayOfGregorianMonth(m,year),year))))
		    val day = (abs - toAbsolute(month,1,year) + 1)
		in
		    (month, day, year)
		end
	    fun wday (month,day,year) =
		let val abs = toAbsolute (month,day,year)
		in
		    InlineT.PolyVector.sub (dayTbl, Int.mod(abs,7))
		end
	    fun yday (month, day, year) = 
		let val abs = toAbsolute (month, day, year)
		    val daysPrior = 
			365 * (year -1)
			+ quot (year-1,4)
			- quot (year-1,100)
			+ quot (year-1,400)
		in 
		    abs - daysPrior - 1    (* to conform to ISO standard *)
		end
	end

	(*
	 * this function should also canonicalize the time (hours, etc...)
	 *)
	fun canonicalizeDate (DATE d) = 
	    let val args = (monthToInt(#month d), #day d, #year d)
		val (monthC,dayC,yearC) = fromAbsolute (toAbsolute (args))
		val yday = yday (args)
		val wday = wday (args)
	    in
		DATE {year = yearC,
		      month = InlineT.PolyVector.sub (monthTbl,monthC),
		      day = dayC,
		      hour = #hour d,
		      minute = #minute d,
		      second = #second d,
		      offset = #offset d,
		      isDst = NONE,
		      yday = yday,
		      wday = wday}
	    end

	fun toTM (DATE d) = {
			     tm_sec = #second d,	(* tm_sec *)
			     tm_min = #minute d,	(* tm_min *)
			     tm_hour = #hour d,		(* tm_hour *)
			     tm_day = #day d,		(* tm_mday *)
			     tm_mon = monthToInt(#month d),	(* tm_mon *)
			     tm_year = #year d - baseYear,	(* tm_year *)
			     tm_wday = dayToInt(#wday d),	(* tm_wday *)
			     tm_yday = 0,		(* tm_yday *)
			     tm_isdst =		        (* tm_isdst *)
                               case (#isDst d)
				 of NONE => ~1
			       | (SOME false) => 0
			       | (SOME true) => 1
				     (* end case *)
				     }

	fun fromTM { tm_sec, tm_min, tm_hour, tm_mday, tm_mon,
		     tm_year, tm_wday, tm_yday, tm_isdst } offset = let
	    val i = Int32.toInt
	in
	    DATE { year = baseYear + i tm_year,
		   month = InlineT.PolyVector.sub(monthTbl, i tm_mon),
		   day = i tm_mday,
		   hour = i tm_hour,
		   minute = i tm_min,
		   second = i tm_sec,
		   wday = InlineT.PolyVector.sub (dayTbl, i tm_wday),
		   yday = i tm_yday,
		   isDst = if ((tm_isdst : Int32.int) < 0) then NONE
			   else SOME(tm_isdst <> 0),
		   offset = offset }
	end

	(* takes two tm's and returns the second tm with 
	 * its dst flag set to the first one's.
	 * Used to compute local offsets 
	 *)
	fun toSameDstTM ({tm_sec, tm_min, tm_hour, tm_mday, tm_mon,
			  tm_year, tm_wday, tm_yday, tm_isdst},
			 {tm_sec=tm_sec', tm_min=tm_min', tm_hour=tm_hour',
                          tm_mday=tm_mday', tm_mon=tm_mon', tm_year=tm_year',
                          tm_wday=tm_wday', tm_yday=tm_yday', tm_isdst=tm_isdst'}) = 
	    {tm_sec=tm_sec', tm_min=tm_min', tm_hour=tm_hour', tm_mday=tm_mday',
             tm_mon=tm_mon', tm_year=tm_year', tm_wday=tm_wday', tm_yday=tm_yday',
             tm_isdst=tm_isdst}

	(* a diff is +/- seconds between local time and gmt
	 * what to add to local time to get gmt
	 *)

	val secInDay = Int32.fromInt(60 * 60 * 24)
	val secInHDay = Int32.fromInt(30 * 30 * 24)
(*
	fun diffToOffset (d) =
	    if (d<0) then Time.fromSeconds (secInDay+d)
	    else Time.fromSeconds(d)
*)
	fun offsetToDiff (off) =
	    let val s = Time.toSeconds (off)
	    in
		if (s>secInHDay) then secInHDay-s else s
	    end

(*
	(* 
	 * this function is meant as an analogue to 
	 * mkTime, but constructs UTC time instead of localtime
	 * idea:  mkTime (localtime(t))= t
	 *        mkGMTime (gmtime(t))= t
	 *)

	fun localDiff (tm) = 
	    let val t = mkTime (tm)
		val loc = localTime (t)
		val gmt = gmTime (t)
	    in
		mkTime (toSameDstTM(loc,gmt)) - mkTime(loc)
	    end

	fun mkGMTime (tm) = mkTime (toSameDstTM (localTime(mkTime(tm)),tm)) -
	    localDiff (tm) 

	fun toSeconds (d) = 
	    let val tm = toTM (d)
	    in
		case (offset d) of
		    NONE => mkTime (tm)
		  | SOME (offsetV) => mkGMTime (tm) + offsetToDiff (offsetV)
	    end
*)

(*
	val toTime = Time.fromSeconds o toSeconds
*)
	fun toTime _ = raise Fail "toTime not yet implemented"

	fun localOffset () = raise Fail "localOffset not yet implemented"

	fun fromTimeLocal (PB.TIME t) =
	    (* fromTM (localTime (Time.toSeconds (t))) NONE *)
	    fromTM (SMLBasis.localTime t) NONE

	fun fromTimeOffset (t, offset) = let
	    val PB.TIME t' = Time.- (t, offset)
	in
	    fromTM (SMLBasis.gmTime t') (SOME offset)
	end

	fun fromTimeUniv (t) = fromTimeOffset (t,Time.zeroTime)

	fun date {year,month,day,hour,minute,second,offset} = 
	    let val d = DATE {second = second,
			      minute = minute,
			      hour = hour,
			      year = year,
			      month = month, 
			      day = day,
			      offset = offset,
			      isDst = NONE,
			      yday = 0,
			      wday = Mon}
		val canonicalDate = canonicalizeDate (d)
		fun internalDate () = 
		    (case (offset) of
			 NONE => fromTimeLocal (toTime canonicalDate)
		       | SOME (offsetV) => fromTimeOffset (toTime canonicalDate,
							   offsetV))
	    in
		internalDate () handle Date => d
	    end

	fun toString d = (* ascTime (toTM d) *)
	    raise Fail "toString not yet implemented"
	    
	fun fmt fmtStr d = (* strfTime (fmtStr, toTM d) *)
	    raise Fail "fmt not yet implemented"
	    
	exception SCAN

        (**
val scan       : (getc : (char, 'a) StringCvt.reader) -> 'a -> (date * 'a) option
	 **)
	fun scan getc charStrm =
	    let val chrLE : (char * char) -> bool = 
		    InlineT.cast InlineT.DfltInt.<=
		fun isDigit c = (chrLE(#"0", c) andalso chrLE(c, #"9"))
		fun incByDigit (n, c) = 
		    10*n + Int.toLarge(Char.ord c - Char.ord #"0")
	    fun scanDigits (0, n, cs) = (n, cs)
	      | scanDigits (k, n, cs) = (* ASSERT: k > 0 *)
		(case (getc cs)
		   of NONE => raise SCAN
		    | (SOME(d, cs')) =>
			if (isDigit d)
			then scanDigits(k-1,incByDigit(n, d), cs')
			else raise SCAN
		  (* end case *))
	    fun scanNum (width,limitOp,cs) =
		let val (n,cs) = scanDigits(width,0,cs)
		 in case limitOp
		      of SOME m => 
			  if n < m then (n,cs)
			  else raise SCAN
		       | NONE => (n,cs)
		end

	    fun scanSp cs = (* check space *)
		(case (getc cs)
		   of SOME(#" ",cs') => cs'
		    | _ => raise SCAN)

	    fun scanCh (ch,cs) = (* check space *)
		(case (getc cs)
		   of SOME(ch',cs') => if ch = ch' then cs' else raise SCAN
		    | NONE => raise SCAN)

	    fun scanWeekDay cs =
		(case (getc cs)
		   of (SOME(#"S", cs')) =>
			(case (getc cs')
			   of SOME (#"u",cs'') => 
			      (case (getc cs'')
				 of SOME (#"n",cs''') => (Sun,cs''')
				  | _ => raise SCAN)
			    | SOME (#"a",cs'') => 
			      (case (getc cs'')
				 of SOME (#"t",cs''') => (Sat,cs''')
				  | _ => raise SCAN)
			    | _ => raise SCAN
			(* end case *))
		    | (SOME(#"M", cs')) =>
			(case (PreBasis.getNChars getc (cs', 2))
			   of (SOME([#"o", #"n"], cs'')) => (Mon,cs'')
			    | _ => raise SCAN
			(* end case *))
		    | (SOME(#"T", cs')) =>
			(case (getc cs')
			   of SOME (#"u",cs'') => 
			      (case (getc cs'')
				 of SOME (#"e",cs''') => (Tue,cs''')
				  | _ => raise SCAN)
			    | SOME (#"h",cs'') => 
			      (case (getc cs'')
				 of SOME (#"u",cs''') => (Thu,cs''')
				  | _ => raise SCAN)
			    | _ => raise SCAN
			(* end case *))
		    | (SOME(#"W", cs')) =>
			(case (PreBasis.getNChars getc (cs', 2))
			   of (SOME([#"e", #"d"], cs'')) => (Wed,cs'')
			    | _ => raise SCAN
			(* end case *))
		    | (SOME(#"F", cs')) =>
			(case (PreBasis.getNChars getc (cs', 2))
			   of (SOME([#"r", #"i"], cs'')) => (Sun,cs'')
			    | _ => raise SCAN
			(* end case *))
		    | _ => raise SCAN
		  (* end case *))

	    fun scanMonth cs =
		(case (getc cs)
		   of (SOME(#"J", cs')) =>
		      (case (getc cs')
			 of SOME (#"a",cs'') => 
			    (case (getc cs'')
			       of SOME (#"n",cs''') => (Jan,cs''')
				| _ => raise SCAN)
			  | SOME (#"u",cs'') => 
			    (case (getc cs'')
			       of SOME (#"n",cs''') => (Jun,cs''')
				| SOME (#"l",cs''') => (Jul,cs''')
				| _ => raise SCAN)
			  | _ => raise SCAN
		      (* end case *))
		  | (SOME(#"F", cs')) =>
		      (case (PreBasis.getNChars getc (cs', 2))
			 of (SOME([#"e", #"b"], cs'')) => (Feb,cs'')
			  | _ => raise SCAN
		      (* end case *))
		  | (SOME(#"M", cs')) =>
		      (case (getc cs')
			 of SOME (#"a",cs'') => 
			    (case (getc cs'')
			       of SOME (#"r",cs''') => (Mar,cs''')
				| SOME (#"y",cs''') => (May,cs''')
				| _ => raise SCAN)
			  | _ => raise SCAN
		      (* end case *))
		  | (SOME(#"A", cs')) =>
		      (case (getc cs')
			 of SOME (#"p",cs'') => 
			    (case (getc cs'')
			       of SOME (#"r",cs''') => (Apr,cs''')
				| _ => raise SCAN)
			  | SOME (#"u",cs'') => 
			    (case (getc cs'')
			       of SOME (#"g",cs''') => (Aug,cs''')
				| _ => raise SCAN)
			  | _ => raise SCAN
		      (* end case *))
		  | (SOME(#"S", cs')) =>
		      (case (PreBasis.getNChars getc (cs', 2))
			 of (SOME([#"e", #"p"], cs'')) => (Sep,cs'')
			  | _ => raise SCAN
		      (* end case *))
		  | (SOME(#"O", cs')) =>
		      (case (PreBasis.getNChars getc (cs', 2))
			 of (SOME([#"c", #"t"], cs'')) => (Oct,cs'')
			  | _ => raise SCAN
		      (* end case *))
		  | (SOME(#"N", cs')) =>
		      (case (PreBasis.getNChars getc (cs', 2))
			 of (SOME([#"o", #"v"], cs'')) => (Nov,cs'')
			  | _ => raise SCAN
		      (* end case *))
		  | (SOME(#"D", cs')) =>
		      (case (PreBasis.getNChars getc (cs', 2))
			 of (SOME([#"e", #"c"], cs'')) => (Dec,cs'')
			  | _ => raise SCAN
		      (* end case *))
		  | _ => raise SCAN
		(* end case *))

	    fun scanDay cs = raise Fail "scanDay not yet implemented"
	    fun scanNat cs = raise Fail "scanNat not yet implemneted"
	    fun scanChar _ _ = raise Fail "scanChar not yet implemented"

	    val cs = charStrm
	    val cs = (PreBasis.skipWS getc cs)
	    val (wday,cs) = scanWeekDay cs
	    val cs = scanSp cs
	    val (month,cs) = scanMonth cs
	    val cs = scanSp cs
	    val (day,cs) = scanDay cs
	    val cs = scanSp cs
	    val (hour,cs) = scanNat(2,SOME 24,cs)
	    val cs = scanChar #":" cs
	    val (minute,cs) = scanNat(2,SOME 60,cs)
	    val cs = scanChar #":" cs
	    val (second,cs) = scanNat(2,SOME 60,cs)
	    val cs = scanSp cs
	    val (year,cs) = scanNum(4,NONE,cs)

	 in SOME(DATE {year = Int32.toInt year,
		       month = month,
		       day = day,
		       hour = hour,
		       minute = minute,
		       second = second,
		       offset = NONE,  (* ??? *)
		       wday = wday,
		       yday = yday(monthToInt month,day,Int32.toInt year),
		       isDst = NONE},  (* ??? *)
		  cs)
	end
	handle SCAN => NONE

	(**
val fromString : string -> date option
         **)
        val fromString = PreBasis.scanString scan

	(* comparison does not take into account the offset
	 * thus, it does not compare dates in different time zones
	 *)
	fun compare (DATE d1, DATE d2) = 
	    let fun cmp (i1::r1, i2::r2) =
		      if (i1 < i2) then LESS
		      else if (i1 = i2) then cmp (r1, r2)
		      else GREATER
		  | cmp _ = EQUAL
	     in cmp ([#year d1, monthToInt(#month d1), #day d1, #hour d1,
		      #minute d1, #second d1],
                     [#year d2, monthToInt(#month d2), #day d2, #hour d2,
		      #minute d2, #second d2])
	    end
      
    end
end

