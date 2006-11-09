(* sparcspec.sml
 *
 * COPYRIGHT (c) 1998 AT&T Bell Laboratories.
 *
 *)

structure SparcSpec : MACH_SPEC = struct

    open DefaultMachSpec

    val architecture	= "sparc"
    val numRegs		= 18 
    val numFloatCalleeSaves = 0 
    val numCalleeSaves = 3
    val numFloatRegs	= 16
    val bigEndian	= true
    val spillAreaSz	= 3800
    val initialSpillOffset = 116 - framesize
    val startgcOffset	= 100 - framesize
    val constBaseRegOffset = 4096

    val ML_STATE_OFFSET = 96 - framesize
    val VProcOffMSP = 4
    val InMLOffVSP = 8
    val LimitPtrMaskOffVSP = 200

    val framePtrNeverVirtual = true	(* we have a real frame ptr! *)
end
