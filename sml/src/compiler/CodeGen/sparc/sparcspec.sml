(* sparcspec.sml
 *
 * COPYRIGHT (c) 1998 AT&T Bell Laboratories.
 *
 *)

structure SparcSpec : MACH_SPEC = 
struct

    open DefaultMachSpec

    val architecture	= "sparc"
    val numRegs		= 18 
    val numFloatCalleeSaves = 0 
    val numCalleeSaves = 3
    val numFloatRegs	= 16
    val bigEndian	= true
    val spillAreaSz	= 3800
    val startgcOffset	= 100
    val pseudoRegOffset = 104
    val constBaseRegOffset = 4096
end

(*
 * $Log$
 *)
