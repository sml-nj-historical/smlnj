(* alpha32glue.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

local
    (* force this module to be dependent on CM module (Blume) *)
    structure CM = Alpha32XCM
    structure CMB = Alpha32XCMB
in
    structure IntAlpha32X = IntShare (structure VC = Alpha32XVisComp)
end

(*
 * $Log$
 *)
