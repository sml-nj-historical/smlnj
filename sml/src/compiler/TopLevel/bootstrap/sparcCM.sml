(* sparcCM.sml
 *
 * COPYRIGHT (c) 1998 Bell Laboratories.
 * (Blume)
 *)
local
    structure L = LinkCM (structure HostCompiler = SparcVisComp)
in
    structure SparcCM = L.CM
    structure SparcCMB = L.CMB
end
