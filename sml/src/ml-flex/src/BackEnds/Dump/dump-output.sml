(* dump-output.sml
 *
 * COPYRIGHT (c) 2005 
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (adrassi@gmail.com)
 * All rights reserved.
 *
 * Dump (to stdout) the complete DFA
 *)

structure DumpOutput : OUTPUT = 
  struct

    structure RE = RegExp
    structure LO = LexOutputSpec

    fun dumpDFA states = let
	  fun nameOf (LO.State{id, ...}) = "Q" ^ Int.toString id
	  fun prState (s as LO.State{id, label, final, next}) = let
		val name = (case final
		       of [] => nameOf s
			| id::_ => concat[nameOf s, " (act ", Int.toString id, ")"]
		      (* end case *))
		fun prEdge (symSet, st) = print(concat[
			"  -- ", RE.toString (RE.mkSymSet symSet), " --> ", nameOf st, "\n"
		      ])
		fun prRE re = print (concat[" ", RE.toString re, "\n"])
		in
		  print(concat[name, ": "(*, RE.toString label*), "\n"]);
		  Vector.app prRE label;
		  List.app prEdge (!next);
		  print "\n"
		end
	  in
	    (List.app prState states;
	     print (Int.toString (List.length states));
	     print " states\n\n")
	  end

    fun output (spec, _) = let
	  val LO.Spec {machines, ...} = spec
	  fun outMachine (LO.Machine {label, states, ...}) =
	        (print "Start state: ";
		 print label; print "\n";
		 dumpDFA states)
          in
	    List.app outMachine machines
          end

  end
