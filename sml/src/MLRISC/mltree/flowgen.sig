signature FLOWGRAPH_GEN = 
sig
  
   structure C : CELLS
   structure I : INSTRUCTIONS
   structure T : MLTREE
   structure B : BLOCK_NAMES
   structure S : INSTRUCTION_STREAM

   sharing I.C = C 
   sharing T.Constant = I.Constant
   sharing T.PseudoOp = S.P
   sharing T.BNames   = B = S.B

   (* 
    * This function creates an instruction stream, which can be 
    * used to emit instruction into the flowgraph
    *)
   val newStream : unit -> (I.instruction,T.mlrisc list,
                            C.regmap * Annotations.annotations) S.stream
end
