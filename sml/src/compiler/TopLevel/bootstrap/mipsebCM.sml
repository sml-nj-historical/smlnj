(* mipsebCM.sml
 *
 * COPYRIGHT (c) 1998 Bell Laboratories.
 *
 *)

local
    structure L = LinkCM (structure HostCompiler = MipsBigVisComp)
in
    structure MipsBigCM = L.CM
    structure MipsBigCMB = L.CMB
end

(*
 * $Log$
 *)
