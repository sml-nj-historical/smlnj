signature FLOWGRAPH_GEN = 
sig
  
   structure C : CELLS
   structure I : INSTRUCTIONS
   structure T : MLTREE

   sharing I.C = C 
   sharing T.Constant = I.Constant

   (* 
    * This function creates an instruction stream, which can be 
    * used to emit instruction into the flowgraph
    *)
   val newStream : unit -> (I.instruction, C.regmap) T.stream
end
