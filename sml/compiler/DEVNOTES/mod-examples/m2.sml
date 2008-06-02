(* 
 * This test case is intended to expose the FLINT/clos/rttype.sml 
 * VAR case bug where it attempts to index a kind environment by 
 * a negative index.
 *
 * The error is in the translation of datatype declarations. A buggy
 * compiler translates the datatype binding into a TVAR instead of a 
 * FIX. 
 *
 *
 *)

functor P(structure A : sig type s end) = struct end
structure B = P(structure A = struct datatype s = S end)

(* 
   The following code triggers a kind check error although the 
   generated code does not seem to be wrong. This is a completely separate 
   issue from the test case above. 

functor P(structure V : sig type s type t val f : s list -> t end) = struct end

structure B = P(structure V = 
		  struct 
		    type s = unit 
		    type t = unit 
		    val f : s list -> t = fn [] => () 
		  end) 
 *)