(* x86spec.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

structure X86Spec : MACH_SPEC =
  struct
    open DefaultMachSpec
    val architecture = "x86"

    val numRegs= 17                (* 5 real/12 virtual *)

    val numFloatRegs = 7
    val numFloatCalleeSaves = 0

    val bigEndian = false
    val spillAreaSz = 0
    val startgcOffset = 32
    val pseudoRegOffset = 0

end

(*
 * $Log: x86spec.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:49  george
 * Version 110.5
 *
 *)
