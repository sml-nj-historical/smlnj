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
    val architecture: string
end


