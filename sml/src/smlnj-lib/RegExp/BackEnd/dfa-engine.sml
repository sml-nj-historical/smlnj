(* dfa-engine.sml
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 * 
 * Implements a matcher engine based on deterministic finite
 * automata.
 *)

structure DfaEngine : REGEXP_ENGINE = 
    struct

	structure D = Dfa
	structure M = MatchTree

	type regexp = D.dfa

	fun compile r = D.build r 
	                handle _ => raise RegExpSyntax.CannotCompile

	(* scan looks at a stream and attempts to match the dfa.
	 * it returns NONE if it fails
	 * it returns SOME (pattern#,Match,rest of stream) upon success
	 *)
	fun scan (regexp,getc,p,stream) = 
	    let val move = D.move regexp
		val accepting = D.accepting regexp
		val canStart = D.canStart regexp  
		fun loop (state,p,inits,lastP,lastS,lastN) = 
		    (case (getc (inits)) 
		       of NONE => (lastP,lastS,lastN)
		        | SOME (c,s') => 
			   (case move (state,c)
			      of NONE => (lastP,lastS,lastN)
			       | SOME (new) => 
				  case (accepting new)
				    of SOME n => loop (new,p+1,s',p+1,s',n)
				     | NONE => loop (new,p+1,s',lastP,lastS,lastN)))
	    in
		case (getc (stream))
		  of NONE => (case (accepting 0)
				of SOME n => SOME (n,M.Match (SOME {pos=stream,len=0},[]),stream)
				 | NONE => NONE)
		   | SOME (c,s') =>
		      if (canStart c) 
			  then let val (last,cs,n) = loop (0,p,stream,~1,stream,0)
			       in
				   if (last<0) 
				       then NONE
				   else SOME (n,M.Match (SOME {pos=stream,
							       len=last-p},
							 []),cs)
			       end
		      else NONE 
	    end

	fun prefix regexp getc stream = case (scan (regexp,getc,0,stream))
					  of NONE => NONE
					   | SOME (n,m,cs) => SOME (m,cs)

	fun find regexp getc stream = 
	    let fun loop (p,s) = (case (scan (regexp,getc,p,s))
				    of NONE => (case (getc (s))
						  of SOME (_,s') => loop (p+1,s')
						   | NONE => NONE)
				     | SOME (n,m,cs) => SOME (m,cs))
	    in
		loop (0,stream)
	    end

	fun match [] = (fn getc => fn stream => NONE)
	  | match l = 
	    let val dfa = D.buildPattern (map #1 l)
		val a = Array.fromList (map (fn (a,b) => b) l)
	    in
		fn getc => fn stream => case (scan (dfa,getc,0,stream))
					  of NONE => NONE
					   | SOME (n,m,cs) => SOME ((Array.sub (a,n)) m,cs)
	    end

    end
