(* getopt.sml
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 * 
 * See comments in getopt-sig.sml
 * 
 *)


structure GetOpt :> GET_OPT = 
  struct

    datatype 'a arg_order
      = RequireOrder		
      | Permute
      | ReturnInOrder of string -> 'a

    datatype 'a arg_descr
      = NoArg of unit -> 'a
      | ReqArg of (string -> 'a) * string
      | OptArg of (string option -> 'a) * string

    type 'a opt_descr = {
        short : string,		
        long : string list,	
        desc : 'a arg_descr,	
        help : string		
      }

    datatype 'a opt_kind 
      = Opt of 'a
      | NonOpt of string
      | EndOfOpts

    structure SS = Substring
    structure S = String


    (* helper functions *)
    fun sepBy (sep,[]) = ""
      | sepBy (sep,x::xs) =
	  concat (x::foldr (fn (elem,l) => sep::elem::l) [] xs)

    val breakeq = SS.splitl (fn #"=" => false | _ => true)


    (* formatting of options *)

    fun fmtShort (NoArg _) so = concat ["-",Char.toString so]
      | fmtShort (ReqArg (_,ad)) so = concat ["-",Char.toString so," ",ad]
      | fmtShort (OptArg (_,ad)) so = concat ["-",Char.toString so,"[",ad,"]"]

    fun fmtLong (NoArg _) lo = concat ["--",lo]
      | fmtLong (ReqArg (_,ad)) lo = concat ["--",lo,"=",ad]
      | fmtLong (OptArg (_,ad)) lo = concat ["--",lo,"[=",ad,"]"]

    fun fmtOpt {short=sos, long=los, desc=ad, help=descr} = (
          sepBy (", ",map (fmtShort ad) (S.explode sos)),
          sepBy (", ",map (fmtLong ad) los),
          descr)

  (* Usage information *)
    fun usageInfo {header, options} = let
          fun unlines l = sepBy ("\n", l)
          val fmtOptions = map fmtOpt options
          val (ms1,ms2,ms3) = foldl
		(fn ((e1,e2,e3), (m1,m2,m3)) => (
		    Int.max (size e1,m1), 
                    Int.max (size e2,m2),
                    Int.max (size e3,m3)
		  )) (0,0,0) fmtOptions
	  val pad = StringCvt.padRight #" "
          val table = foldr
		(fn ((e1,e2,e3),l) => concat [
		      "  ", pad ms1 e1, "  ", pad ms2 e2, "  ", pad ms3 e3
		    ] :: l
		  ) [] fmtOptions
          in
            unlines (header::table)
          end



    (* entry point of the library
     *)
 
    fun getOpt {argOrder, options : 'a opt_descr list, errFn} = let
       (* Some error handling functions *)
	  fun errAmbig optStr = errFn(usageInfo{
		  header = concat[
		      "option `", optStr, "' is ambiguous; could be one of:"
		    ],
		  options = options
		})
	  fun errReq (d, optStr) = errFn(concat[
		  "option `", optStr, "' requires an argument ", d
		])
	  fun errUnrec optStr = errFn(concat[
		  "unrecognized option `", optStr, "'"
		])
	  fun errNoArg optStr = errFn(concat[
		  "option `", optStr, "' does not allow an argument"
		])
	(* handle long option
	 * this is messy because you cannot pattern-match on substrings
	 *)
	  fun longOpt (subs, rest, optDescr : 'a opt_descr list) = let
		val (opt,arg) = breakeq subs
        	val opt' = SS.string opt
        	val options = List.filter
		      (fn {long,...} => List.exists (S.isPrefix opt') long)
			optDescr
        	val optStr = "--"^opt'
        	fun long (_::(_::_), _, rest') = (
		      errAmbig optStr; (NonOpt optStr, rest'))
        	  | long ([NoArg a], x, rest') = 
                      if (SS.isEmpty x)
                	then (Opt(a()),rest')
                      else if (SS.isPrefix "=" x) 
                	then (errNoArg optStr; (NonOpt optStr, rest'))
                	else raise Fail "long: impossible"
        	  | long ([ReqArg(f,d)], x, []) = 
                      if (SS.isEmpty x)
                	then (errReq(d, optStr); (NonOpt optStr, []))
                      else if (SS.isPrefix "=" x)
                	then (Opt(f (SS.string (SS.triml 1 x))), [])
                	else raise Fail "long: impossible"
        	  | long ([ReqArg(f,d)], x, rest' as (r::rs)) = 
                      if (SS.isEmpty x)
                	then (Opt(f r), rs)
                      else if (SS.isPrefix "=" x)
                	then (Opt(f (SS.string (SS.triml 1 x))), rest')
                	else raise Fail "long: impossible"
        	  | long ([OptArg(f,_)], x, rest') = 
                      if (SS.isEmpty x)
                	then (Opt(f NONE), rest')
                      else if (SS.isPrefix "=" x)
                	then (Opt(f (SOME (SS.string (SS.triml 1 x)))), rest')
                	else raise Fail "long: impossible"
        	  | long ([], _, rest') = (
		      errUnrec optStr; (NonOpt optStr, rest'))
        	in
                  long (map #desc options, arg, rest)
        	end
	(* handle short option *)
	  fun shortOpt (x, subs, rest, optDescr : 'a opt_descr list) = let 
        	val options =
		      List.filter (fn {short,...} => Char.contains short x) optDescr
        	val ads = map #desc options
        	val optStr = "-"^(Char.toString x)
        	fun short (_::_::_, _, rest1) = (
		      errAmbig optStr; (NonOpt optStr, rest1))
        	  | short ((NoArg a)::_, y, rest') = 
                      if (SS.isEmpty y)
                	then (Opt(a()), rest')
                	else (Opt(a()), ("-"^(SS.string y))::rest')
        	  | short ((ReqArg (f,d))::_, y, []) = 
                      if (SS.isEmpty y) 
                	then (errReq(d, optStr); (NonOpt optStr, []))
                	else (Opt(f (SS.string y)), [])
        	  | short ((ReqArg(f,_))::_, y, rest' as (r::rs)) = 
                      if (SS.isEmpty y)
                	then (Opt(f r), rs)
                	else (Opt(f (SS.string y)), rest')
        	  | short ((OptArg(f,_))::_, y, rest') = 
                      if (SS.isEmpty y)
                	then (Opt(f NONE), rest')
                	else (Opt(f (SOME (SS.string y))), rest')
        	  | short ([], y, rest') =
                      if (SS.isEmpty y)
                	then (errUnrec optStr; (NonOpt optStr, rest'))
                	else (
			  errUnrec optStr;
			  (NonOpt optStr, ("-" ^ SS.string y)::rest'))
        	in
        	  short (ads, subs, rest)
        	end
	(* take a look at the next command line argument and decide what to
	 * do with it
	 *)
	  fun getNext ([], _) = raise Fail "getNext: impossible"
	    | getNext ("--" :: rest, _) = (EndOfOpts, rest)
	    | getNext (x::rest, optDescr) =  let
		val x' = SS.all x
		in
		  if (SS.isPrefix "--" x')
		    then longOpt (SS.triml 2 x', rest, optDescr)
        	  else if (SS.isPrefix "-" x')
		    then shortOpt (SS.sub(x',1), SS.triml 2 x', rest, optDescr)
        	  else (NonOpt x,rest)
		end
	  fun get [] = ([], [])
	    | get args = let
        	val (opt, rest) = getNext (args, options)
        	val (os, xs) = get rest
        	fun procNextOpt (Opt o', _) = (o'::os, xs)
        	  | procNextOpt (NonOpt x, RequireOrder) = ([],x::rest)
        	  | procNextOpt (NonOpt x, Permute) = (os,x::xs)
        	  | procNextOpt (NonOpt x, ReturnInOrder f) = ((f x)::os,xs)
        	  | procNextOpt (EndOfOpts, RequireOrder) = ([],rest)
        	  | procNextOpt (EndOfOpts, Permute) = ([],rest)
        	  | procNextOpt (EndOfOpts, ReturnInOrder f) = (map f rest,[])
        	in
        	  procNextOpt(opt, argOrder)
        	end
	  in
	    get
	  end (* getOpt *)

  end

