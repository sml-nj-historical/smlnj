(* hppaCM.sml
 *
 * COPYRIGHT (c) 1998 Bell Laboratories.
 *
 *)

local
    structure L = LinkCM (structure HostCompiler = HppaVisComp)
in
    structure HppaCM = L.CM
    structure HppaCMB = L.CMB
end

(*
 * $Log$
 *)
