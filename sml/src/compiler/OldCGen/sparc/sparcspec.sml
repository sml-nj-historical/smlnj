(* sparcspec.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

structure SparcSpec : MACH_SPEC =
struct
  open DefaultMachSpec

  val architecture = "sparc"
  val numRegs = 15
  val numFloatCalleeSaves = 0
  val numFloatRegs = 16
  val bigEndian = true
  val spillAreaSz = 0
  val startgcOffset = 100
  val pseudoRegOffset = 104
end

(*
 * $Log: sparcspec.sml,v $
 * Revision 1.1.1.1  1997/01/14 01:38:46  george
 *   Version 109.24
 *
 *)
