(* check-placement-fn.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *
 * This functor implements code to check that a block placement is
 * correct.
 *)

functor CheckPlacementFn (CFG : CONTROL_FLOW_GRAPH) : sig

    structure CFG : CONTROL_FLOW_GRAPH

    val check : (CFG.cfg * CFG.node list) -> unit

  end = struct

    structure CFG=CFG
    structure G = Graph

    val dumpStrm = MLRiscControl.debug_stream

    fun blockToString (id', CFG.BLOCK{id, ...}) =
	  concat["<", Int.toString id', ":", Int.toString id, ">"]

    fun check (cfg as G.GRAPH graph, blocks) = let
	(* an array that maps from block id to position in the placement (starting
	 * from 1).  Nodes that have no placement have index ~1.
	 *)
	  val order = let
		val arr = Array.array(#capacity graph (), ~1)
		fun init ((id, _), i) = (Array.update(arr, id, i); i+1)
		in
		  ignore (List.foldl init 1 blocks);
		  arr
		end
	  fun adjacentNodes (a, b) = (Array.sub(order, a) + 1 = Array.sub(order, b))
	  val anyErrors = ref false
	(* report an error and dump the cfg *)
	  fun reportError (src, dst) = let
		fun say s = TextIO.output(!dumpStrm, s)
		fun b2s id = concat[
			Int.toString id, "@", Int.toString(Array.sub(order, id))
		      ]
		in
		  if !anyErrors
		    then ()
		    else (
		      anyErrors := true;
		      say "********** Bogus block placement **********\n");
		  say(concat[
		      "  Blocks ", b2s src, " and ", b2s dst,
		      "are not adjacent\n"
		    ])
		end
	(* return true if the edge must connect adjacent nodes *)
	  fun adjEdge (CFG.EDGE{k=CFG.FALLSTHRU, ...}) = true
	    | adjEdge (CFG.EDGE{k=CFG.BRANCH false, ...}) = true
	    | adjEdge _ = false
	(* check that FALLSTHRU and BRANCH false edges connect adjacent nodes *)
	  fun chkEdge (src, dst, info) =
		if adjEdge info andalso not(adjacentNodes(src, dst))
		  then reportError(src, dst)
		  else ()
	  in
	    #forall_edges graph chkEdge;
	    if (!anyErrors)
	      then let
		fun say s = TextIO.output(!dumpStrm, s)
		val prBlock = CFG.dumpBlock (!dumpStrm, cfg)
		in
		  say "Block placement order:\n";
		  List.app
		    (fn b => say(concat["  ", blockToString b, "\n"]))
		      blocks;
		  TextIO.output(!dumpStrm, "[ control-flow-graph ]\n");
		  List.app prBlock blocks;
		  say "**********\n";
		  MLRiscErrorMsg.error ("CheckPlacementFn", "bogus placement")
		end
	      else ()
	  end

  end
