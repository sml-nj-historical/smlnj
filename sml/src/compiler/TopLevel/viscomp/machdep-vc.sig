(*
 * Copyright 1998 by Bell Laboratories
 *  machdep-vc.sig -- machine dependent part of viscomp
 *
 * by Matthias Blume (10/1998)
 *)

signature MACHDEP_VC = sig
    structure Profile : PROFILE
    structure Binfile: BINFILE
    structure CMSA: CMSA
    structure Compile : COMPILE
    structure Interact : INTERACT
    structure Control : CONTROL
    val architecture: string
end


(*
 * $Log: machdep-vc.sig,v $
 * Revision 1.2  1998/12/22 17:02:18  jhr
 *   Merged in 110.10 changes from Yale.
 *
 *)
