(* alpha32spec.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

structure Alpha32XSpec : MACH_SPEC = 
struct
 
  open DefaultMachSpec

  val architecture = "alpha32x"
  val bigEndian = false
  val spillAreaSz = 4000
  val numRegs = 17
  val numFloatRegs = 29
  val numFloatCalleeSaves = 0
  val startgcOffset = 8
  val pseudoRegOffset = 112
  val constBaseRegOffset = 32768
end


(*
 * $Log: alpha32xSpec.sml,v $
 * Revision 1.1.1.1  1997/01/14 01:38:09  george
 *   Version 109.24
 *
 *)
