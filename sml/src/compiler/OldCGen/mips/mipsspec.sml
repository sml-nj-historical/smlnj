(* mipsspec.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

functor MipsSpec(E: ENDIAN) : MACH_SPEC =
  struct
    open DefaultMachSpec 

    val architecture = E.architecture
    val bigEndian = E.bigEndian
    val spillAreaSz = 0
    val numRegs = 15
    val numFloatRegs = 15
    val numFloatCalleeSaves=0
    val startgcOffset = 4  
    val pseudoRegOffset = 16
  end

(*
 * $Log$
 *)
