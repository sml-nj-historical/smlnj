(* bug243.sml *)
(* Compiler bug: Parse.includeSig.newTyc. *)

(*$DECTREE_DT*)
(* Just the bare datatype for decision trees. *)

signature DECTREE_DT =
  sig
    type lab
    type lvar
    type longvar
    type longcon
    type longexcon
    type scon
    type pat
    type type_info
    type RuleNum = int
    type Decision
    type (''a, 'b) map

    datatype 'a option = NONE | SOME of 'a

    datatype DecisionTree =
        LAB_DECOMPOSE of {bind: lvar,
			  parent: lvar,
			  lab: lab,
			  child: DecisionTree,
			  info: type_info
			 }
      | CON_DECOMPOSE of {bind: lvar, parent: lvar, child: DecisionTree}
      | EXCON_DECOMPOSE of {bind: lvar, parent: lvar, child: DecisionTree}

      | CON_SWITCH of {arg: lvar,
		       selections: (longcon, DecisionTree) map,
		       wildcard: DecisionTree option,
				(* An `option' because we may notice that all
				   the constructors are present. *)
		       info: type_info
		      }
      | SCON_SWITCH of {arg: lvar,
			selections: (scon, DecisionTree) map,
			wildcard: DecisionTree
		       }
      | EXCON_SWITCH of {arg: lvar,
			 selections: (longexcon * DecisionTree) list,
			 wildcard: DecisionTree
			}

      | END of {ruleNum: RuleNum,
		environment: (longvar, lvar) map
	       }

      | FAIL
  end;

(*$MATCH_COMPILER: DECTREE_DT*)

(* The match compiler interface; the actual match compiler is built from
   a number of sub-functors, but this top-level interface is the only one
   which the rest of the compiler cares about. Given a series of patterns,
   it returns a DecisionTree (which is essentially an abstract form of the
   final lambda-code), in which all the lvars for identifiers and temporaries
   have been generated. At each leaf of the decision tree, there's a
   single rule number (the rule reached by this series of decisions), and an
   environment from identifiers to lvars, which is used to compile the
   right-hand-side expression for this rule. Nice, huh? *)

signature MATCH_COMPILER =
  sig
    include DECTREE_DT

    val matchCompiler:
      lvar * pat list * {warnInexhaustive: bool, warnNoBindings: bool}
      -> DecisionTree			(* these flags are set when the
					   warnings are required. *)

    type StringTree
    val layoutDecisionTree: DecisionTree -> StringTree
  end;

