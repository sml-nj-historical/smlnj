(* cfgExpandCopies.sml
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *
 * This module expands all parallel copies into normal instructions
 *)

functor CFGExpandCopies
   (structure CFG    : CONTROL_FLOW_GRAPH
    structure ExpandCopies : EXPAND_COPIES
    			where I = CFG.I
   ) : CFG_OPTIMIZATION =
  struct
    structure CFG = CFG

    val name = "expand copies"

    fun run (cfg as Graph.GRAPH graph) = let
	  fun expand (_, CFG.BLOCK{insns, ...}) =
	        insns := List.foldr
		  (fn (i, rest) => List.revAppend(ExpandCopies.expandCopies i,rest))
		  []
                  (!insns)
	  in
	    #forall_nodes graph expand;
	    cfg
	  end

  end
