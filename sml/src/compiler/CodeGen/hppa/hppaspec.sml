(* machspec.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure HppaSpec : MACH_SPEC = 
struct

    open DefaultMachSpec

    val architecture	= "hppa"
    val spillAreaSz	= 4000
    val numRegs		= 17	(* length HppaCpsRegs.miscregs + 3 *)
    val numFloatRegs	= 25
      (* length HppaCpsRegs.floatregs + length HppaCpsRegs.savedfpregs *)
    val bigEndian	= true
    val startgcOffset	= ~28
    val constBaseRegOffset = 8192
end

(*
 * $Log: hppaspec.sml,v $
 * Revision 1.3  1998/05/23 14:09:25  george
 *   Fixed RCS keyword syntax
 *
 *)
