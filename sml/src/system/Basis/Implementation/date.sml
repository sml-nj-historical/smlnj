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

	datatype month =
		 Jan | Feb | Mar
	       | Apr | May | Jun
	       | Jul | Aug | Sep
	       | Oct | Nov | Dec

	datatype date =
		 DATE of { year : int,
			   month : month,
			   day : int,
			   hour : int,
			   minute : int,
			   second : int,
			   offset : Time.time option,
			   wday : weekday,
			   yday : int,
			   isDst : bool option }

	local
	    (* tables for mapping integers to days/months *)
	    val dayTbl = #[Sun, Mon, Tue, Wed, Thu, Fri, Sat]
	    val monthTbl = #[Jan, Feb, Mar, Apr, May, Jun,
			     Jul, Aug, Sep, Oct, Nov, Dec]

	    (* tables for mapping integers to day/month-strings *)
	    val str_dayTbl = #["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
	    val str_monthTbl = #["Jan", "Feb", "Mar", "Apr", "May", "Jun",
				 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]

	    (* the tuple type used to communicate with C; this 9-tuple has the
	     * fields:
	     * tm_sec, tm_min, tm_hour, tm_mday, tm_mon, tm_year, tm_wday,
	     * tm_yday, and tm_isdst.
	     *)
            type tm = SMLBasis.Date_t


	    (* 
	     * This code is taken from Reingold's paper
	     *)
	    infix // %%
	    val op // = Int.quot
	    val op %% = Int.rem

	    fun sum (f,k,p) = 
		let fun loop (f,i,p,acc) = if (not(p(i))) then acc
					   else loop(f,i+1,p,acc+f(i))
		in
		    loop (f,k,p,0)
		end

	    fun lastDayOfGregorianMonth (month,year) =
		if month = 1 andalso 
		   (year %% 4) = 0 andalso
		   let val m = year %% 400
		   in
		       not (m = 100 orelse m = 200 orelse m = 300)
		   end
		then 29
		else List.nth ([31,28,31,30,31,30,31,31,30,31,30,31],month)

	    fun toAbsolute (month, day, year) = let
		val year1 = year - 1
	    in
		day  
		+ sum (fn (m) => lastDayOfGregorianMonth(m,year),0,
		       fn (m) => (m<month)) 
		+ 365 * year1
		+ (year1 // 4)
		- (year1 // 100)
		+ (year1 // 400)
	    end

	    fun fromAbsolute abs =
		let val approx = abs // 366
		    val year =
			approx +
			sum(fn _ => 1,
			    approx, 
			    fn y => abs >= toAbsolute (0, 1, y+1))
		    val month =
			sum (fn _ =>1,
			     0,
			     fn m => abs > toAbsolute(m,lastDayOfGregorianMonth(m,year),year))
		    val day = abs - toAbsolute (month, 1, year) + 1
		in
		    (month, day, year)
		end

	    fun wday (month,day,year) =
		let val abs = toAbsolute (month,day,year)
		in
		    InlineT.PolyVector.sub (dayTbl, abs %% 7)
		end

	    fun yday (month, day, year) = 
		let val abs = toAbsolute (month, day, year)
		    val year1 = year - 1
		    val daysPrior = 
			365 * year1
			+ (year1 // 4)
			- (year1 // 100)
			+ (year1 // 400)
		in 
		    abs - daysPrior - 1    (* to conform to ISO standard *)
		end

	    fun dayToInt Sun = 0
	      | dayToInt Mon = 1
	      | dayToInt Tue = 2
	      | dayToInt Wed = 3
	      | dayToInt Thu = 4
	      | dayToInt Fri = 5
	      | dayToInt Sat = 6

	    (* careful about this: the month numbers are 0-11 *)
	    fun monthToInt Jan = 0
	      | monthToInt Feb = 1
	      | monthToInt Mar = 2
	      | monthToInt Apr = 3
	      | monthToInt May = 4
	      | monthToInt Jun = 5
	      | monthToInt Jul = 6
	      | monthToInt Aug = 7
	      | monthToInt Sep = 8
	      | monthToInt Oct = 9
	      | monthToInt Nov = 10
	      | monthToInt Dec = 11

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

	    fun toTM (DATE d) = let
		val i = Int32.fromInt
		fun f sel = i (sel d)
	    in
		{ tm_sec = f #second,
		  tm_min = f #minute,
		  tm_hour = f #hour,
		  tm_mday = f #day,
		  tm_mon = f (monthToInt o #month),
		  tm_year = i (#year d - baseYear),
		  tm_wday = f (dayToInt o #wday),
		  tm_yday = 0 : Int32.int,
		  tm_isdst = case #isDst d of
				 NONE => ~1 : Int32.int
			       | SOME false => 0
			       | SOME true => 1 } : SMLBasis.Date_t
	    end

	    fun fromTM { tm_sec, tm_min, tm_hour, tm_mday, tm_mon,
			 tm_year, tm_wday, tm_yday, tm_isdst } offset =
		let val i = Int32.toInt
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
	    fun toSameDstTM (tm : tm, tm' : tm) =
		(* tm' with { tm_isdst = #tm_isdst tm } *)
		{ tm_sec = #tm_sec tm',
		  tm_min = #tm_min tm',
		  tm_hour = #tm_hour tm',
		  tm_mday = #tm_mday tm',
		  tm_mon = #tm_mon tm',
		  tm_year = #tm_year tm',
		  tm_wday = #tm_wday tm',
		  tm_yday = #tm_yday tm',
		  tm_isdst = #tm_isdst tm }
	in

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

    (* FIXME: needs support from IDL basis!!! *)
	fun toTime_local d = PB.TIME (SMLBasis.mkTime (toTM d))

	fun fromTimeLocal (PB.TIME t) =
	    fromTM (SMLBasis.localTime t) NONE

	fun fromUTC (PB.TIME t) =
	    fromTM (SMLBasis.gmTime t) (SOME Time.zeroTime)

	fun localOffset () =
	    (* trick: - take arbitrary time t (e.g., 0),
	     *        - interpret it as UTC and make a date tm from it
	     *        - set the date's offset to "local", yielding tm'
	     *        - convert tm' to a time u
	     *        - return difference between u and t *)
	    Time.- (toTime_local (fromUTC Time.zeroTime), Time.zeroTime)

	fun toTime (d as DATE dt) = let
	    val lt = toTime_local d
	in
	    case #offset dt of
		NONE => lt
	      | SOME off => Time.+ (lt, localOffset ())
	end

	fun date { year, month, day, hour, minute, second, offset} = 
	    let val d = DATE {second = second,
			      minute = minute,
			      hour = hour,
			      year = year,
			      month = month, 
			      day = day,
			      offset = NONE,
			      isDst = NONE,
			      yday = 0,
			      wday = Mon}
		val cd = canonicalizeDate d
		fun internalDate () =
		    case offset of
			NONE => fromTimeLocal (toTime cd) (* why  not cd ?? *)
		      | SOME off => let
			    val PB.TIME t = Time.- (toTime cd, off)
			in
			    fromTM (SMLBasis.gmTime t) (SOME off)
			end
	    in
		internalDate () handle Date => d
	    end

	fun toString (DATE d) = let
	    fun dd sel = let
		val i = sel d
		val s = IntImp.toString i
	    in
		if size s = 1 then "0" ^ s else s
	    end
	in
	    concat [Vector.sub (str_dayTbl, dayToInt (#wday d)), " ",
		    Vector.sub (str_monthTbl, monthToInt (#month d)), " ",
		    dd #day, " ",
		    dd #hour, ":", dd #minute, ":", dd #second, " ",
		    Int.toString (#year d)]
	end

    (* FIXME: need support from IDL basis for this... *)
	fun fmt fmtStr d = (* strfTime (fmtStr, toTM d) *)
	    raise Fail "fmt not yet implemented"

	(* Scanning in fairly high-level style. *)
	fun scan gc = let
	    (* "bind" for the "option monad" etc. *)
	    infix >>= ?>=
	    fun NONE >>= f = NONE
	      | (SOME x) >>= f = f x
	    fun false ?>= f = NONE
	      | true ?>= f = f ()

	    (* see if we can match any of the given keywords.
	     * if so, then invoke associated continuation.
	     *    n -- size of all keywords
	     *    kws -- list of pairs (keyword, continuation)
	     *           continuation takes stream state
	     *    ss -- initial stream state *)
	    fun select n kws ss = let
		(* given current position i, current character c, current
		 * stream state ss, thin out the list of keywords
		 * according to whether or not they match c at i *)
		fun thin (i, _, ss, [], l) = next (ss, i + 1, l)
		  | thin (i, c, ss, (kw, k) :: l, r) =
		    thin (i, c, ss, l,
			  if StringImp.sub (kw, i) = c then (kw, k) :: r
			  else r)

		(* consider next i *)
		and next (_, _, []) = NONE (* no matching keywords left *)
		  | next (ss, i, l as ((_, k) :: _)) =
		    if i >= n then k ss	(* done matching, invoke first k *)
		    else gc ss >>= (fn (c, ss) => thin (i, c, ss, l, []))
	    in
		next (ss, 0, kws)
	    end

	    (* digit value *)
	    fun dg d = ord d - ord #"0"

	    (* get a two-digit integer followed by tc *)
	    fun dd tc ss =
		gc ss >>=
		(fn (c1, ss) =>
		    Char.isDigit c1 ?>=
		    (fn () =>
			gc ss >>=
			   (fn (c2, ss) =>
			       Char.isDigit c2 ?>=
			       (fn () =>
				   gc ss >>=
				      (fn (c3, ss) =>
					  (c3 = tc) ?>=
					  (fn () =>
					      SOME (10 * dg c1 + dg c2,
						    ss)))))))

	    (* when we have everything but the year... *)
	    fun allbutyear (wd, m, md, hr, min) (sec, ss) = let
		(* when we are done... *)
		fun some (y, ss) =
		    SOME (date { year = y,
				 month = m,
				 day = md,
				 hour = hr,
				 minute = min,
				 second = sec,
				 offset = NONE },
			  ss)
		(* gobbling up the digits that constitute the year *)
		fun loop (y, ss) =
		    case gc ss of
			NONE => some (y, ss)
		      | SOME (c, ss') =>
			if Char.isDigit c then loop (10 * y + dg c, ss')
			else some (y, ss)
	    in
		(* get first digit if possible, then more *)
		gc ss >>=
		(fn (c, ss) =>
		    Char.isDigit c ?>= (fn () => loop (dg c, ss)))
	    end

	    (* when we have seen weekday and month *)
	    fun month wd m ss =
		dd #" " ss >>=
		(fn (md, ss) =>
		    dd #":" ss >>=
		    (fn (hr, ss) =>
			dd #":" ss >>=
			(fn (min, ss) =>
			    dd #" " ss >>=
			       allbutyear (wd, m, md, hr, min))))

	    (* after we have seen the weekday, let's get the month *)
	    fun wday wd =
		select 3 [("Jan ", month wd Jan),
			  ("Feb ", month wd Feb),
			  ("Mar ", month wd Mar),
			  ("Apr ", month wd Apr),
			  ("May ", month wd May),
			  ("Jun ", month wd Jun),
			  ("Jul ", month wd Jul),
			  ("Aug ", month wd Aug),
			  ("Sep ", month wd Sep),
			  ("Oct ", month wd Oct),
			  ("Nov ", month wd Nov),
			  ("Dec ", month wd Dec)]

	in
	    (* at the start, expect to see a weekday *)
	    select 3 [("Sun ", wday Sun),
		      ("Mon ", wday Mon),
		      ("Tue ", wday Tue),
		      ("Wed ", wday Wed),
		      ("Thu ", wday Thu),
		      ("Fri ", wday Fri),
		      ("Sat ", wday Sat)]
	end

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
      
	end (*local*)
  end (* struct *)
end (* local *)
