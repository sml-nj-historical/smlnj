(* backend/x86-ccall.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
local
    (* turn on "fast-fp"... *)
    val _ = MLRiscControl.getFlag "x86-fast-fp" := true
in
structure X86CCallBackend = BackendFn (structure M = X86MC
				       val cproto_conv = "ccall")
end
