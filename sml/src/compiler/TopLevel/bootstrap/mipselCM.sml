(* mipselCM.sml
 *
 * COPYRIGHT (c) 1998 Bell Laboratories.
 *
 *)

local
    structure L = LinkCM (structure HostCompiler = MipsLittleVisComp)
in
    structure MipsLittleCM = L.CM
    structure MipsLittleCMB = L.CMB
end

(*
 * $Log$
 *)
