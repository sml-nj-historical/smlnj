(*
 * This module manages the spill/reload process. 
 * 
 * -- Allen
 *)
signature RA_SPILL =
sig

   structure I : INSTRUCTIONS
   structure C : CELLS
      sharing I.C = C

   type copyInstr =
          (C.cell list * C.cell list) * I.instruction -> I.instruction

   type spill =
      {instr    : I.instruction,       (* instruction where spill is to occur *)
       reg      : C.cell,              (* register to spill *)
       spillLoc : int,                 (* logical spill location *)
       graph    : RAGraph.interferenceGraph,  (* the current graph *)
       kill     : bool,                (* can we kill the current node? *)
       regmap   : C.cell -> C.cell,    (* current register map *)
       annotations : Annotations.annotations ref  (* annotations *)
      } ->
      {code     : I.instruction list,  (* spill code *)
       proh     : C.cell list,         (* prohibited from future spilling *)
       instr    : I.instruction option (* possibly changed instruction *)
      }

   type reload =
      {instr    : I.instruction,       (* instruction where spill is to occur *)
       reg      : C.cell,              (* register to spill *)
       spillLoc : int,                 (* logical spill location *)
       graph    : RAGraph.interferenceGraph,  (* the current graph *)
       regmap   : C.cell -> C.cell,    (* current register map *)
       annotations : Annotations.annotations ref  (* annotations *)
      } ->
      {code     : I.instruction list,  (* reload code *)
       proh     : C.cell list          (* prohibited from future spilling *)
      }

   (*
    * The following function rewrites an instruction and insert
    * spill and reload code around it.   The list of spill and reload
    * registers may have duplicates.
    *)
   val spillRewrite : 
        { graph     : RAGraph.interferenceGraph,
          spill     : spill,
          reload    : reload, 
          copyInstr : copyInstr,
          cellkind  : C.cellkind
        } -> 
        { spillRegs   : C.cell list,   (* registers to spill *)
          killRegs    : C.cell list,   (* registers to kill *)
          reloadRegs  : C.cell list,   (* registers to reload *)  
          instr       : I.instruction, (* instruction to process *)
          annotations : Annotations.annotations ref (* annotations *)
        } -> 
        { code       : I.instruction list (* instruction sequence after
                                           * rewriting
                                           *)
        }

end
