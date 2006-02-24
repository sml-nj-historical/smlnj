(* lex-output-spec.sml
 *
 * COPYRIGHT (c) 2005 
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (adrassi@gmail.com)
 * All rights reserved.
 *
 * Specification produced by LexGen
 *)

structure LexOutputSpec = 
  struct

    datatype dfa_state
      = State of {
	  id : int,
	  label : RegExp.re Vector.vector,
	  final : int list,	(* rule vector indices *)
	  next :  (RegExp.sym_set * dfa_state) list ref
	}

  (* a "machine" corresponds to a start state: each
   * start state will end up with its own DFA 
   *)
    datatype machine = Machine of {
        label : string,
				(* re * action vector index *)
	rules : (RegExp.re * int) vector,	
	states : dfa_state list
      }

    type action = string

    datatype spec = Spec of {
	decls : string,
	header : string,
	arg : string,
	actions : action vector,
	machines : machine list
      }

  end
