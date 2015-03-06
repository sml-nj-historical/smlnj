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

    exception Error

    fun parse ln = let
	  fun splitNext start = let
		fun extract ss = SS.string(SS.trimr (SS.size ss) start)
		fun scan (ss, quoted) = (case (SS.getc ss, quoted)
		       of (NONE, false) => (extract ss, ss)
			| (NONE, true) => raise Error
			| (SOME(#",", ss'), false) => (extract ss, ss')
			| (SOME(#"\"", ss'), true) => (case SS.getc ss
			     of NONE => (extract ss, ss')
			      | SOME(#",", ss') => (extract ss, ss')
			      | _ => raise Error
			    (* end case *))
			| (SOME(#"\"", ss'), false) => scan(ss', quoted) (* error? *)
			| (SOME(_, ss'), _) => scan(ss', quoted)
		      (* end case *))
		in
		  case SS.getc start
		   of SOME(#"\"", ss') => scan(ss', true)
		    | _ => scan (start, false)
		  (* end case *)
		end
	  and scanLine (ss, fields) = if SS.isEmpty ss
		then List.rev fields
		else let
		  val (fld, ss) = splitNext ss
		  in
		    scanLine (ss, fld::fields)
		  end
	  in
	    SOME(scanLine (SS.full ln, []))
	      handle Error => NONE
	  end

  (* convert a CSV line to a sequence of its fields; returns NONE on error *)
    fun fromString s = parse s

    val hasComma = CharVector.exists (fn #"," => true | _ => false)

    fun cvtField s = if hasComma s
	  then concat["\"", s, "\""]  (* what if it has a quote? *)
	  else s

  (* convert a sequence to a string *)
    fun toString flds = String.concatWith "," (List.map cvtField flds)

    fun fmt cvt = let
	  fun cvt' x = cvtField (cvt x)
	  in
	    fn flds => String.concatWith "," (List.map cvt' flds)
	  end

  end
