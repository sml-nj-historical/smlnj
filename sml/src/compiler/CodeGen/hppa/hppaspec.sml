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
    val numRegs		= 18 (* length HppaCpsRegs.miscregs + 3 *)
    val numFloatRegs	= 25
      (* length HppaCpsRegs.floatregs + length HppaCpsRegs.savedfpregs *)
    val bigEndian	= true
    val startgcOffset	= ~28
    val pseudoRegOffset = ~36
    val constBaseRegOffset = 8192
end

(*
 * $Log: hppaspec.sml,v $
 * Revision 1.2  1998/02/13 17:21:08  george
 *   Functorized pseudoOps over the machine spec to get access to the
 *   Tag structure.
 *
 * Revision 1.1.1.1  1997/01/14 01:38:38  george
 *   Version 109.24
 *
 *)
