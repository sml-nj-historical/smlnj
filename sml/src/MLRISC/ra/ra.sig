(*
 * The interface to the new register allocator.
 *
 * -- Allen
 *)
signature RA =
sig

   structure I : INSTRUCTIONS
   structure C : CELLS
   structure F : RA_FLOWGRAPH 
      sharing F.I = I
      sharing I.C = C

   datatype mode = REGISTER_ALLOCATION | COPY_PROPAGATION

   datatype optimization = DEAD_COPY_ELIM
                         | SPILL_COALESCING
                         | SPILL_COLORING

   type getreg = { pref  : C.cell list,
                   stamp : int, 
                   proh  : int Array.array
                 } -> C.cell

   (*
    * Perform register allocation.
    *
    * spillProh is a list of register ranges (inclusive) that cannot be spilled.
    *
    *)
   val ra : mode -> 
            { cellkind     : C.cellkind,             (* kind of register *)
              spillProh    : (C.cell * C.cell) list, (* don't spill these *)
              K            : int,                    (* number of colors *)
              dedicated    : bool Array.array,       (* dedicated registers *)
              getreg       : getreg,                 (* how to find a color *)
              copyInstr    : F.Spill.copyInstr,      (* how to make a copy *)
              spill        : F.Spill.spill,          (* spill callback *)
              reload       : F.Spill.reload,         (* reload callback *)
              optimizations: optimization list       (* optimizations *)
            } list -> F.flowgraph -> F.flowgraph

end
