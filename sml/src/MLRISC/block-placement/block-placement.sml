functor BlockPlacement 
   (structure CFG : CONTROL_FLOW_GRAPH
    structure Props : INSN_PROPERTIES)

   : BLOCK_PLACEMENT =

struct
  structure CFG = CFG

  structure DefaultPlacement = DefaultBlockPlacement(CFG)

  structure WeightedPlacement = 
     WeightedBlockPlacementFn
	  (structure CFG = CFG 
	   structure InsnProps = Props)

  val placementFlag = MLRiscControl.mkFlag
			  ("weighted-block-placement",
			   "whether MLRISC does weigted block placement")

  fun blockPlacement(cfg as Graph.GRAPH G) =
  let val CFG.INFO{reorder, ...} = #graph_info G
  in  if !placementFlag orelse !reorder
      then WeightedPlacement.blockPlacement cfg
      else DefaultPlacement.blockPlacement cfg
  end
end
