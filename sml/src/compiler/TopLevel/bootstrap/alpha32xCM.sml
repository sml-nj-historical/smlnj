(* alpha32xCM.sml
 *
 * COPYRIGHT (c) 1998 Bell Laboratories.
 *
 *)

local
    structure L = LinkCM (structure HostCompiler = Alpha32XVisComp)
in
    structure Alpha32XCM = L.CM
    structure Alpha32XCMB = L.CMB
end

(*
 * $Log$
 *)
