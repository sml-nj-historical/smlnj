signature FLOWGRAPH_GEN = 
sig
  
   structure C : CELLS
   structure I : INSTRUCTIONS
   structure T : MLTREE

   sharing I.C = C 
   sharing T.Constant = I.Constant

   type flowgraph 

   (* 
    * This function takes two arguments (compile, flowgraph)
    * creates an instruction stream, which can be 
    * used to emit instruction into the flowgraph.  For each flowgraph
    * constructed, the function compile is invoked.   
    *
    * If a flowgraph is supplied as an argument, then it will be used 
    * and the instructions generated are added to the flowgraph incrementally;
    * otherwise, a new empty flowgraph is created.    
    *)
   val newStream : { compile   : flowgraph -> unit,
                     flowgraph : flowgraph option
                   } -> (I.instruction, C.regmap) T.stream
end
