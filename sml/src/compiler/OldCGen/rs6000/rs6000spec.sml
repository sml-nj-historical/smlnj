(* rs6000spec.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

structure RS6000Spec : MACH_SPEC =
struct
  open DefaultMachSpec

  val architecture = "rs6000"
  val spillAreaSz = 0
  val numRegs = 17
  val numFloatRegs = 31
  val numFloatCalleeSaves = 0
  val bigEndian = true
  val startgcOffset = 4
  val pseudoRegOffset = 40

end

(*
 * $Log: rs6000spec.sml,v $
 * Revision 1.1.1.1  1997/01/14 01:38:45  george
 *   Version 109.24
 *
 *)
