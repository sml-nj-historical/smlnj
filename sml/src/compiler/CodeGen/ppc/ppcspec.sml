(* PPCspec.sml
 *
 * COPYRIGHT (c) 1999 Bell Laboratories.
 *
 *)

structure PPCSpec : MACH_SPEC = 
struct
 
  open DefaultMachSpec

  val architecture = "ppc"
  val bigEndian = false
  val spillAreaSz = 4096-144
  val initialSpillOffset = 144
  val numRegs = 15
  val numFloatRegs = 30
  val numFloatCalleeSaves = 0
  val startgcOffset =	4		(* from runtime *)
  val constBaseRegOffset = 32764
end

