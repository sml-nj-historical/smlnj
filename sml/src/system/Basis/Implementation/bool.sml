(* bool.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *)

structure Bool : BOOL =
  struct

    datatype bool = datatype PrimTypes.bool

    val not = InlineT.inlnot

  (* DBM: generalized to be case insensitive, as per new Basis spec;
   *   note spaces after "|" because of bug 1610.
   *)
    fun scan (getc : (char, 'a) StringCvt.reader) cs = (
	  case (getc (PreBasis.skipWS getc cs))
	   of (SOME((#"t"| #"T"), cs')) => (case (PreBasis.getNChars getc (cs', 3))
		 of (SOME([(#"r"| "#R"), (#"u"| #"U"), (#"e"| #"E")], cs'')) =>
                      SOME(true, cs'')
		  | _ => NONE
		(* end case *))
	    | (SOME((#"f"| #"F"), cs')) => (case (PreBasis.getNChars getc (cs', 4))
		 of (SOME([(#"a"| #"A"), (#"l"| #"L"), (#"s"| #"S"), (#"e"| #"E")], cs'')) =>
                      SOME(false, cs'')
		  | _ => NONE
		(* end case *))
	    | _ => NONE
	  (* end case *))

    fun toString true = "true"
      | toString false = "false"
    val fromString = PreBasis.scanString scan

  end


