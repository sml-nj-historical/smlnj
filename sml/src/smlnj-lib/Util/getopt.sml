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
        = NoArg of 'a
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
          
      structure Ss = Substring
      structure S = String
          
          
      (* helper functions *)
      fun sepBy (sep,[]) = ""
        | sepBy (sep,x::xs) = concat (x::foldr (fn (elem,l) => sep::elem::l) 
                                      [] xs)
          
      val breakeq = Ss.splitl (fn #"=" => false | _ => true)
            
          
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
          
      fun usageInfo header optDescr = let
            fun unlines l = sepBy ("\n",l)
            val fmtOptions = map fmtOpt optDescr
            val (ms1,ms2,ms3) = foldl (fn ((elem1,elem2,elem3),
                                           (currMax1,currMax2,currMax3)) =>
                                                let val s1 = size elem1
                                                    val s2 = size elem2
                                                    val s3 = size elem3
                                                in
                                                    (Int.max (s1,currMax1), 
                                                     Int.max (s2,currMax2),
                                                     Int.max (s3,currMax3))
                                                end) (0,0,0) fmtOptions
            val table = foldr (fn ((elem1,elem2,elem3),l) =>
                               concat ["  ", StringCvt.padRight #" " ms1 elem1,
                                       "  ", StringCvt.padRight #" " ms2 elem2,
                                       "  ", StringCvt.padRight #" " ms3 elem3]::l) [] fmtOptions
          in
              unlines (header::table)
          end
    

      (* Some error handling functions *)

      fun errAmbig ods optStr = let
              val header = concat ["option `",optStr,"' is ambiguous; could be one of:"]
          in
              raise Fail (usageInfo header ods)
          end
                            
      fun errReq d optStr =
	  raise Fail (concat ["option `",optStr,"' requires an argument ",d])
          
      fun errUnrec optStr =
	  raise Fail (concat ["unrecognized option `",optStr,"'"])
          
      fun errNoArg optStr =
	  raise Fail (concat ["option `",optStr,"' doesn't allow an argument"])
          

    (* handle long option
     * 
     * this is messy because you cannot pattern-match on substrings
     *)

    fun longOpt subs rest optDescr = let val (opt,arg) = breakeq subs
            val opt' = Ss.string opt
            val options = List.filter (fn {long,...} =>
                                       List.exists (S.isPrefix opt') long) optDescr
            val ads = map (fn {desc,...} => desc) options
            val optStr = "--"^opt'
            fun long (_::(_::_)) _ rest' = (errAmbig options optStr, rest')
              | long [NoArg a] x rest' = 
                   if (Ss.isEmpty x)
                     then (Opt a,rest')
                   else if (Ss.isPrefix "=" x) 
                     then (errNoArg optStr,rest')
                   else raise Fail "long: impossible"
              | long [ReqArg (f,d)] x [] = 
                   if (Ss.isEmpty x)
                     then (errReq d optStr,[])
                   else if (Ss.isPrefix "=" x)
                     then (Opt (f (Ss.string (Ss.triml 1 x))), [])
                   else raise Fail "long: impossible"
              | long [ReqArg (f,d)] x (rest' as (r::rs)) = 
                   if (Ss.isEmpty x)
                     then (Opt (f r), rs)
                   else if (Ss.isPrefix "=" x)
                     then (Opt (f (Ss.string (Ss.triml 1 x))), rest')
                   else raise Fail "long: impossible"
              | long [OptArg (f,_)] x rest' = 
                   if (Ss.isEmpty x)
                     then (Opt (f NONE), rest')
                   else if (Ss.isPrefix "=" x)
                     then (Opt (f (SOME (Ss.string (Ss.triml 1 x)))), rest')
                   else raise Fail "long: impossible"
              | long [] _ rest' = (errUnrec optStr,rest')
            in
                long ads arg rest
            end


        
    (* handle short option
     *)
 
    fun shortOpt x subs rest optDescr = let 
            val options =
		  List.filter (fn {short,...} => Char.contains short x) optDescr
            val ads = map (fn {desc,...} => desc) options
            val optStr = "-"^(Char.toString x)
            fun short (_::_::_) _ rest1 = (errAmbig options optStr,rest1)
              | short ((NoArg a)::_) y rest' = 
                   if (Ss.isEmpty y)
                     then (Opt a, rest')
                   else (Opt a, ("-"^(Ss.string y))::rest')
              | short ((ReqArg (f,d))::_) y [] = 
                   if (Ss.isEmpty y) 
                     then (errReq d optStr, [])
                   else (Opt (f (Ss.string y)), [])
              | short ((ReqArg (f,_))::_) y (rest' as (r::rs)) = 
                   if (Ss.isEmpty y)
                     then (Opt (f r), rs)
                   else (Opt (f (Ss.string y)), rest')
              | short ((OptArg (f,_))::_) y rest' = 
                   if (Ss.isEmpty y)
                     then (Opt (f NONE), rest')
                   else (Opt (f (SOME (Ss.string y))), rest')
              | short [] y rest' =
                   if (Ss.isEmpty y)
                     then (errUnrec optStr, rest')
                   else (errUnrec optStr, ("-"^(Ss.string y))::rest')
        in
            short ads subs rest
        end


    (* take a look at the next command line argument and decide what to
     * do with it
     *)

    fun getNext [] _ = raise Fail "getNext: impossible"
      | getNext ("--" :: rest) _ = (EndOfOpts,rest)
      | getNext (x::rest) optDescr =  let
	  val x' = Ss.all x
	  in
	    if (Ss.isPrefix "--" x')
	      then longOpt (Ss.triml 2 x') rest optDescr
            else if (Ss.isPrefix "-" x')
	      then shortOpt (Ss.sub(x',1)) (Ss.triml 2 x') rest optDescr
            else (NonOpt x,rest)
	  end

    (* entry point of the library
     *)
 
    fun getOpt _ _ [] = ([],[])
      | getOpt ordering optDescr args = let 
            val (opt,rest) = getNext args optDescr
            val (os,xs) = getOpt ordering optDescr rest
            fun procNextOpt (Opt o') _ = (o'::os,xs)
              | procNextOpt (NonOpt x) RequireOrder = ([],x::rest)
              | procNextOpt (NonOpt x) Permute = (os,x::xs)
              | procNextOpt (NonOpt x) (ReturnInOrder f) = ((f x)::os,xs)
              | procNextOpt EndOfOpts RequireOrder = ([],rest)
              | procNextOpt EndOfOpts Permute = ([],rest)
              | procNextOpt EndOfOpts (ReturnInOrder f) = (map f rest,[])
        in
            procNextOpt opt ordering
        end
    
  end

