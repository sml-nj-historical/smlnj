(* Copyright 1989 by AT&T Bell Laboratories *)
(* basics/pputil.sml *)

structure PPUtil : PPUTIL =
struct

  structure S : SYMBOL = Symbol
  structure PP = PrettyPrint
  structure IP = InvPath
  structure SP = SymPath

  val pps = PP.add_string

  fun ppSequence0 ppstream (sep:PP.ppstream->unit,pr,elems) =
      let fun prElems [el] = pr ppstream el
	    | prElems (el::rest) =
	        (pr ppstream el;
		 sep ppstream;
                 prElems rest)
	    | prElems [] = ()
       in prElems elems
      end

  fun ppSequence ppstream {sep:PP.ppstream->unit, pr:PP.ppstream->'a->unit, 
                           style:PP.break_style} (elems: 'a list) =
      (PP.begin_block ppstream style 0;
       ppSequence0 ppstream (sep,pr,elems);
       PP.end_block ppstream)

  fun ppClosedSequence ppstream{front:PP.ppstream->unit,sep:PP.ppstream->unit,
                               back:PP.ppstream->unit,pr:PP.ppstream->'a->unit,
                                style:PP.break_style} (elems:'a list) =
      (PP.begin_block ppstream PP.CONSISTENT 0;
       front ppstream;
       PP.begin_block ppstream style 0;
       ppSequence0 ppstream (sep,pr,elems); 
       PP.end_block ppstream;
       back ppstream;
       PP.end_block ppstream)

  fun ppSym ppstream (s:S.symbol) = PP.add_string ppstream (S.name s)

  val stringDepth = Control.Print.stringDepth

(** NOTE: this duplicates code in basics/printutil.sml **)
  fun escape i = let
	val m = Int.toString
	in
	  concat ["\\", m(i div 100), m((i div 10)mod 10), m(i mod 10)]
	end
  val offset = Char.ord #"A" - Char.ord #"\^A"
  fun ml_char #"\n" = "\\n"
    | ml_char #"\t" = "\\t"
    | ml_char #"\\" = "\\\\"
    | ml_char #"\"" = "\\\""
    | ml_char c = if ((c >= #"\^A") andalso (c <= #"\^Z"))
	  then "\\^" ^ String.str(Char.chr(Char.ord c + offset))
	else if ((#" " <= c) andalso (c <= #"~"))
	  then String.str c
	  else escape(Char.ord c)

  fun mlstr s = concat["\"", concat(map ml_char (explode s)), "\""]

  fun pp_mlstr ppstream s =
      let val depth = !stringDepth
          val add_string = PP.add_string ppstream
	  fun pr i =
	      if i=depth then add_string "#"
	      else (let val ch = String.sub(s,i)
		    in  add_string (ml_char ch); pr (i+1)
		    end handle Substring => ())
       in add_string "\""; pr 0; add_string "\""
      end

  fun ppvseq ppstream ind (sep:string) pr elems =
      let fun prElems [el] = pr ppstream el
	    | prElems (el::rest) = (pr ppstream el; 
                                    PP.add_string ppstream sep; 
                                    PP.add_newline ppstream;
                                    prElems rest)
	    | prElems [] = ()
       in PP.begin_block ppstream PP.CONSISTENT ind;
          prElems elems;
          PP.end_block ppstream
      end

  fun ppvlist ppstrm (header,separator,pr_item,items) =
      case items
	of nil => ()
	 | first::rest =>
	     (PP.add_string ppstrm header;
	      pr_item ppstrm first;
	      app (fn x => (PP.add_newline ppstrm;
			    PP.add_string ppstrm separator;
			    pr_item ppstrm x))
		   rest)

  (* debug print functions *)
  fun ppIntPath ppstream =
      ppClosedSequence ppstream 
	{front=(fn pps => PP.add_string pps "["),
	 sep=(fn pps => (PP.add_string pps ","; PP.add_break pps (0,0))),
	 back=(fn pps => PP.add_string pps "]"),
	 style=PP.INCONSISTENT,
	 pr=(fn pps => PP.add_string pps o Int.toString)}

  fun ppSymPath ppstream (sp: SymPath.path) = 
      PP.add_string ppstream (SymPath.toString sp)

  fun ppInvPath ppstream (InvPath.IPATH path: InvPath.path) =
      ppClosedSequence ppstream 
	{front=(fn pps => PP.add_string pps "<"),
	 sep=(fn pps => (PP.add_string pps ".")),
	 back=(fn pps => PP.add_string pps ">"),
	 style=PP.INCONSISTENT,
	 pr=ppSym}
        path


  (* findPath:  convert inverse symbolic path names to a printable string in the
    context of an environment.

    Its arguments are the inverse symbolic path, a check predicate on static
    semantic values, and a lookup function mapping paths to their bindings
    (if any) in an environment and raising Env.Unbound on paths with no
    binding.

    It looks up each suffix of the path name, going from shortest to longest
    suffix, in the current environment until it finds one whose lookup value
    satisfies the check predicate.  It then converts that suffix to a string.
    If it doesn't find any suffix, the full path (reversed, i.e. in the 
    normal order) and the boolean value false are returned, otherwise the
    suffix and true are returned.

    Example:
	   Given A.B.t as a path, and a lookup function for an
	   environment, this function tries:
		     t
		     B.t
		     A.B.t
	   If none of these work, it returns ?.A.B.t

    Note: the symbolic path is passed in reverse order because that is
    the way all symbolic path names are stored within static semantic objects.
   *)

  val resultId = S.strSymbol "<resultStr>"
  val returnId = S.strSymbol "<returnStr>"

  fun findPath (IP.IPATH p: IP.path, check, look): (S.symbol list * bool) =
      let fun try(name::untried,tried) =
	        (if (S.eq(name,resultId)) orelse (S.eq(name,returnId)) 
		 then try(untried,tried)
		 else
		   let val elem = look(SP.SPATH(name :: tried))
		    in if check elem
		       then (name::tried,true)
		       else try(untried,name::tried)
		   end handle Env.Unbound => try(untried,name::tried))
	    | try([],tried) = (tried, false)
       in try(p,[])
      end


  fun ppi ppstrm (i:int) = pps ppstrm (Int.toString i)

  fun add_comma ppstrm = pps ppstrm ","

  fun add_comma_nl ppstrm  = (add_comma ppstrm; PP.add_newline ppstrm)

  fun nl_indent ppstrm i =
      let val {linewidth,...} = PP.dest_ppstream ppstrm 
       in PP.add_break ppstrm (linewidth,i)
      end

  fun nl_app ppstrm f =
      let fun g [] = ()
	    | g [el] = f ppstrm el
	    | g (el::rst) = (f ppstrm el; PP.add_newline ppstrm; g rst)
       in g
      end

  fun br_app ppstrm f =
      let fun g [] = ()
	    | g [el] = f ppstrm el
	    | g (el::rst) = (f ppstrm el; PP.add_break ppstrm (1,0); g rst)
       in g
      end

  fun en_pp ppstrm =
      {begin_block = PrettyPrint.begin_block ppstrm, 
       end_block = fn () => PrettyPrint.end_block ppstrm,
       pps = PrettyPrint.add_string ppstrm,
       add_break = PrettyPrint.add_break ppstrm,
       add_newline = fn () => PrettyPrint.add_newline ppstrm};

  fun ppArray ppstrm (f:PP.ppstream -> 'a -> unit, a:'a array) =
      let val {begin_block,pps,add_break,end_block,...} = en_pp ppstrm
	  fun loop i = 
	      let val elem = Array.sub(a,i)
	       in pps (Int.toString i);
		  pps ": "; 
		  f ppstrm elem;
		  add_break (1,0);
		  loop (i+1)
	      end
       in begin_block PP.INCONSISTENT 0;
	  loop 0 handle General.Subscript => ();
	  end_block()
      end

  fun C f x y = f y x;

  fun ppTuple ppstrm f =
      ppClosedSequence ppstrm 
	{front=C pps "(",
	 sep=fn ppstrm => (pps ppstrm ","; PP.add_break ppstrm (0,0)),
	 back=C pps ")",
	 pr=f, style=PP.INCONSISTENT}


end (* structure PPUtil *)

(*
 * $Log: pputil.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:16  george
 * Version 110.5
 *
 *)
