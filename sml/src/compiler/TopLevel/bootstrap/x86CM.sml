(* x86CM.sml
 *
 * COPYRIGHT (c) 1998 Bell Laboratories.
 *
 *)

local
    structure L = LinkCM (structure HostCompiler = X86VisComp)
in
    structure X86CM = L.CM
    structure X86CMB = L.CMB
end

(*
 * $Log$
 *)
