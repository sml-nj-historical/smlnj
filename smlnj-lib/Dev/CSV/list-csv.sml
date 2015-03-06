(* list-csv.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Support for reading and writing comma-separated-value files.
 *)

structure ListCSV : CSV where type 'a seq = 'a list =
  struct

    structure SS = Substring

    type 'a seq = 'a list

    fun parse ln = let
	  fun splitNext (start, ss) = let
		fun extract ss = SS.string(SS.trimr (SS.size ss) start)
		fun scan (ss, quoted) = (case (SS.getc ss, quoted)
		       of (NONE, false) => extract ss
			| (NONE, true) => (* error *)
			| (SOME(#",", ss'), false) => (* terminate *)
			| (SOME(#"\"", ss'), true) => (* terminate *)
			| (SOME(#"\"", ss'), false) => (* error? *)
			| (SOME(_, ss'), _) => splitNext (start, ss')
		      (* end case *))
		in
		  case getc ss
		   of SOME(#"\"", ss') =>
		    | _ => 
		  (* end case *)
		end
	  in
	  end

  end
