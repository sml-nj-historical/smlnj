(* alpha32CM.sml
 *
 * COPYRIGHT (c) 1998 Bell Laboratories.
 * (Blume)
 *)
local
    structure L = LinkCM (structure HostCompiler = Alpha32VisComp)
in
    structure Alpha32CM = L.CM
    structure Alpha32CMB = L.CMB
end
