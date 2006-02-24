(* main.sml
 *
 * COPYRIGHT (c) 2005 
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (adrassi@gmail.com)
 * All rights reserved.
 *
 * Driver for ml-flex
 *)

structure Main = 
  struct

    structure RE = RegExp
    structure Lex = LexGen
    structure LO = LexOutputSpec

    fun debug s = (print s; print "\n")

  (* command-line parameters *)
    datatype options = 
	Opt of {
	    fname : string ref,
	    report : bool ref,
	    dump : bool ref,
	    dot : bool ref,
	    match : bool ref
          }

  (* count the total number of DFA states *)
    fun numStates (LO.Spec{machines, ...}) = let
	  fun mSz (LO.Machine {states, ...}) = List.length states
	  in 
            List.foldl op+ 0 (List.map mSz machines)
	  end

  (* print a short report of start states *)
    fun prReport (spec as LO.Spec{machines, ...}) = let
          fun prMach (LO.Machine {label, rules, states}) = 
	        print (concat 
		  [label, ":\t",
		   Int.toString (List.length states), "\n"])
          in
            List.app prMach machines;
	    print (concat ["TOTAL:\t", Int.toString (numStates spec), "\n"])
          end

    fun mlFlex (Opt {fname, report, dot, dump, match}) = let
          val _ = if String.size (!fname) = 0 
		  then (print "No input file specified (usage: ml-flex [options] file)\n";
			OS.Process.exit OS.Process.failure)
		  else ()
	  val _ = debug "[ml-flex: parsing]"
          val inSpec = MLLexInput.parseFile (!fname)
	  val _ = debug "[ml-flex: DFA gen]"
	  val outSpec = Lex.gen inSpec
	  val _ = if !report then
		    (debug "[ml-flex: DFA report]";
		     prReport outSpec)
		  else 
		    (debug (concat [" ", Int.toString (numStates outSpec),
				    " states in full DFA"]))
	  val _ = if !dump then
		    (debug "[ml-flex: DFA dump]";
		     DumpOutput.output (outSpec, !fname))
		  else ()
	  val _ = if !dot then
		    (debug "[ml-flex: DOT gen]";
		     DotOutput.output (outSpec, !fname))
		  else ()
	  val _ = debug "[ml-flex: SML gen]"
	  val _ = SMLFunOutput.output (outSpec, !fname)
	  val _ = if !match then 
		    (debug "-- Interactive matching (blank line to quit) --";
		     Match.output (outSpec, !fname))
		  else ()
	  in
            OS.Process.success
          end
	    handle ex => (
	      TextIO.output(TextIO.stdErr, concat[
		  "uncaught exception ", General.exnName ex,
		  " [", General.exnMessage ex, "]\n"
	        ]);
	      app (fn s => TextIO.output(TextIO.stdErr, concat[
		  "  raised at ", s, "\n"
	        ]))
	        (SMLofNJ.exnHistory ex);
	      OS.Process.exit OS.Process.failure)

    fun procArgs (Opt {fname, report, dot, dump, match}) arg = 
	  (case arg
	    of "--report" => report := true
	     | "--dot"    => dot := true
	     | "--dump"	  => dump := true
	     | "--match"  => match := true
	     | file	  => 
	         if String.size (!fname) > 0 
		 then 
		   (print "Only one input file may be specified\n";
		    OS.Process.exit OS.Process.failure)
		 else fname := file
	   (* end case *))

    fun main (_, args) = let
          val opt = Opt {fname = ref "", report = ref false, 
			 dot = ref false, dump = ref false, 
			 match = ref false}
	  val _ = List.app (procArgs opt) args
	  in 
	    mlFlex opt
          end

  end
