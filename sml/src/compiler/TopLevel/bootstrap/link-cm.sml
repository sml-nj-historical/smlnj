(* link-cm.sml
 *
 * COPYRIGHT (c) 1998 Bell Laboratories.
 * (Blume)
 *)
local
    structure L = LinkCM (structure HostMachDepVC = MachDepVC)
in
    structure CM = L.CM
    structure CMB = L.CMB
end
