(* match.sml
 *
 * COPYRIGHT (c) 2005 
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (adrassi@gmail.com)
 * All rights reserved.
 *
 * A simple match "backend" that runs the produced state machine directly
 * on stdin.  Treats end of line as end of input.  Note that a match only
 * occurs if the machine is in an accepting state after consuming the 
 * complete input; in particular, the input is meant to represent a single
 * token, and the machine does not restart until the end of input.
 *)

structure Match : OUTPUT = 
  struct

    structure SIS = RegExp.SymSet
    structure LO = LexOutputSpec

    fun match (LO.State{id, label, final, next}, []) = final
      | match (LO.State{id, label, final, next}, sym::r) = let
	  fun goto [] = []
	    | goto ((syms, s)::r') = 
	        if SIS.member (syms, sym) 
		then match(s, r) 
		else goto r'
	  in
	    goto (!next)
	  end

    fun matchLoop (res, states) = (case TextIO.inputLine (TextIO.stdIn)
	  of NONE => ()
	   | SOME "\n" => ()
	   | SOME s => let
	       val chars = List.rev (List.tl (List.rev (String.explode s)))
	       val syms = List.map 
			    (Word32.fromInt o Char.ord) 
			    chars
	       val _ = case match (hd states, syms)
			of [] => print "-- No match --\n"
			 | i::_ => 
			     (print "-- Match: ";
			      print (RegExp.toString (Vector.sub (res, i)));
			      print " --\n")
	       in
	       (* continue I/O loop *)
	         matchLoop (res, states) 
	       end
	 (* end case *))

    fun output (LO.Spec {machines, ...}, _) = let
        (* just use first start state in the list *)
	  val LO.Machine {rules, states, ...} = List.hd machines 
	  in
	    matchLoop(Vector.map #1 rules, states)
	  end

  end
