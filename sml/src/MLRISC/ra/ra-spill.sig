(*
 * This module manages the spill/reload process. 
 * 
 * -- Allen
 *)
signature RA_SPILL =
sig

   structure I : INSTRUCTIONS
   structure C : CELLS
   structure G : RA_GRAPH = RAGraph
      sharing I.C = C

   type copyInstr =
          (C.cell list * C.cell list) * I.instruction -> I.instruction list

   (*
    * Spill the value associated with reg into spillLoc.
    * All definitions of instr should be renamed to a new temporary newReg. 
    *)
   type spill =
      {instr    : I.instruction,       (* instruction where spill is to occur *)
       reg      : C.cell,              (* register to spill *)
       spillLoc : int,                 (* logical spill location *)
       kill     : bool,                (* can we kill the current node? *)
       regmap   : C.cell -> C.cell,    (* current register map *)
       annotations : Annotations.annotations ref (* annotations *)
      } ->
      {code     : I.instruction list,  (* instruction + spill code *)
       proh     : C.cell list,         (* prohibited from future spilling *)
       newReg   : C.cell option        (* the spilled value is available here *)
      }

   (* Spill the register src into spillLoc.
    * The value is originally from register reg.
    *)
   type spillSrc =
      {src      : C.cell,              (* register to spill from *)
       reg      : C.cell,              (* the register *)
       spillLoc : int,                 (* logical spill location *)
       annotations : Annotations.annotations ref (* annotations *)
      } -> I.instruction list          (* spill code *)

   (*
    * Spill the temporary associated with a copy into spillLoc
    *)
   type spillCopyTmp =
      {copy     : I.instruction,       (* copy to spill *)
       spillLoc : int,                 (* logical spill location *)
       annotations : Annotations.annotations ref (* annotations *)
      } -> I.instruction               (* spill code *)

   (*
    * Reload the value associated with reg from spillLoc.
    * All uses of instr should be renamed to a new temporary newReg.
    *)
   type reload =
      {instr    : I.instruction,       (* instruction where spill is to occur *)
       reg      : C.cell,              (* register to spill *)
       spillLoc : int,                 (* logical spill location *)
       regmap   : C.cell -> C.cell,    (* current register map *)
       annotations : Annotations.annotations ref (* annotations *)
      } ->
      {code     : I.instruction list,  (* instr + reload code *)
       proh     : C.cell list,         (* prohibited from future spilling *)
       newReg   : C.cell option        (* the reloaded value is here *)
      }

   (*
    * Rename all uses fromSrc to toSrc
    *)
   type renameSrc =
      {instr    : I.instruction,       (* instruction where spill is to occur *)
       fromSrc  : C.cell,              (* register to rename *)
       toSrc    : C.cell,              (* register to rename to *)
       regmap   : C.cell -> C.cell     (* current register map *)
      } ->
      {code     : I.instruction list,  (* renamed instr *)
       proh     : C.cell list,         (* prohibited from future spilling *)
       newReg   : C.cell option        (* the renamed value is here *)
      }

   (* Reload the register dst from spillLoc. 
    * The value is originally from register reg.
    *)
   type reloadDst =
      {dst      : C.cell,              (* register to reload to *)
       reg      : C.cell,              (* the register *)
       spillLoc : int,                 (* logical spill location *)
       annotations : Annotations.annotations ref (* annotations *)
      } -> I.instruction list          (* reload code *)

   (*
    * The following function rewrites an instruction and insert
    * spill and reload code around it.   The list of spill and reload
    * registers may have duplicates.
    *)
   val spillRewrite : 
        { graph        : G.interferenceGraph,
          spill        : spill,
          spillSrc     : spillSrc,
          spillCopyTmp : spillCopyTmp,
          reload       : reload, 
          reloadDst    : reloadDst, 
          renameSrc    : renameSrc, 
          copyInstr    : copyInstr,
          cellkind     : C.cellkind,
          spillSet     : C.cell list Intmap.intmap,
          reloadSet    : C.cell list Intmap.intmap,
          killSet      : C.cell list Intmap.intmap
        } -> 
        { pt          : int,                         (* starting program pt *)
          annotations : Annotations.annotations ref, (* annotations *)
          instrs      : I.instruction list           (* instructions to spill *)
        } -> 
          I.instruction list (* instruction sequence after rewriting *)
          (* Note, instructions are in reverse order *)

end
