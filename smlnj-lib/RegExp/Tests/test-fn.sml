(* test-fn.sml
 *
 * COPYRIGHT (c) 2008 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

functor TestFn (
    val engineName : string
    structure RE : REGEXP
  ) = struct

    structure M = MatchTree

    fun getc s i = if (i < String.size s) then SOME(String.sub(s, i), i+1) else NONE

    fun test (name, re, data) = let
	  val re = RE.compileString re
	  in
	    print(concat["  ", name, ": "]);
	    case RE.find re (getc data) 0
	     of NONE => print "match failed\n"
	      | SOME(M.Match({pos, len}, _), _) =>
		  print(concat[
		      "match at ", Int.toString pos, " = \"",
		      String.toString(String.substring(data, pos, len)), "\"\n"
		    ])
	    (* end case *)
	  end

    fun doTests () = (
	  print(concat["  testing ", engineName, "\n"]);
	  test ("01", "[0-9]+", "abc123xyz");
	  test ("02", "^[0-9]+", "abc123def\n987xyz");
	  test ("03", "[0-9]+$", "abc123def\n987xyz456");
	  print "** tests done\n")

  end
