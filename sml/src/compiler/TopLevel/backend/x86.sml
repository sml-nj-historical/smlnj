(* backend/x86.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
local
    (* turn on "fast-fp"... *)
(* Allen says that it is not safe yet...
    val _ = MLRiscControl.getFlag "x86-fast-fp" := true
*)
in
structure X86Backend = BackendFn (X86MC)
end
