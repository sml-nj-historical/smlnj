functor RASpillTypes(I : INSTRUCTIONS) =
struct

   structure G = RAGraph
   structure C = I.C

   type copyInstr =
          (C.cell list * C.cell list) * I.instruction -> I.instruction list

   (*
    * Spill the value associated with reg into spillLoc.
    * All definitions of instr should be renamed to a new temporary newReg. 
    *)
   type spill =
      {instr    : I.instruction,       (* instruction where spill is to occur *)
       reg      : C.cell,              (* register to spill *)
       spillLoc : G.spillLoc,          (* logical spill location *)
       kill     : bool,                (* can we kill the current node? *)
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
       spillLoc : G.spillLoc,         (* logical spill location *)
       annotations : Annotations.annotations ref (* annotations *)
      } -> I.instruction list          (* spill code *)

   (*
    * Spill the temporary associated with a copy into spillLoc
    *)
   type spillCopyTmp =
      {copy     : I.instruction,       (* copy to spill *)
       reg      : C.cell,              (* the register *)
       spillLoc : G.spillLoc,          (* logical spill location *)
       annotations : Annotations.annotations ref (* annotations *)
      } -> I.instruction               (* spill code *)

   (*
    * Reload the value associated with reg from spillLoc.
    * All uses of instr should be renamed to a new temporary newReg.
    *)
   type reload =
      {instr    : I.instruction,       (* instruction where spill is to occur *)
       reg      : C.cell,              (* register to spill *)
       spillLoc : G.spillLoc,          (* logical spill location *)
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
       toSrc    : C.cell               (* register to rename to *)
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
       spillLoc : G.spillLoc,          (* logical spill location *)
       annotations : Annotations.annotations ref (* annotations *)
      } -> I.instruction list          (* reload code *)

end
