(* rs6000CM.sml
 *
 * COPYRIGHT (c) 1998 Bell Laboratories.
 *
 *)

local
    structure L = LinkCM (structure HostCompiler = RS6000VisComp)
in
    structure RS6000CM = L.CM
    structure RS6000CMB = L.CMB
end

(*
 * $Log$
 *)
