(*
 * Abstract view a flowgraph required by the new register allocator.
 * In order to allow different representation to share the same 
 * register allocator core, each representation should implement the
 * following interface to talk to the new RA.
 *
 * -- Allen
 *)

signature RA_FLOWGRAPH =
sig

   structure I     : INSTRUCTIONS
   structure C     : CELLS  
   structure G     : RA_GRAPH = RAGraph
   structure Spill : RA_SPILL
   structure W     : FREQ
     sharing Spill.I = I
     sharing I.C = C 
  

   type flowgraph

    (* Extract the regmap from the flowgraph *)
   val regmap : flowgraph -> C.regmap

    (* Dump the flograph to a stream *)
   val dumpFlowgraph : string * flowgraph * TextIO.outstream -> unit

    (*
     * Interface for communicating with the new register allocator.
     * It is expected that the service provide will cache enough information
     * during build so that the rebuild and split phases can be execute
     * quickly.
     *)
   val services : flowgraph ->
       { build   : G.interferenceGraph * C.cellkind-> 
                      G.move list, (* build the graph *)
         spill   : {copyInstr : Spill.copyInstr,
                    spill     : Spill.spill,
                    reload    : Spill.reload,
                    graph     : G.interferenceGraph,
                    nodes     : G.node list,
                    cellkind  : C.cellkind
                   } -> G.move list, 
                     (* spill/rebuild the graph *)
         freq    : G.programPoint -> W.freq (* execution frequency *)
       }

end
