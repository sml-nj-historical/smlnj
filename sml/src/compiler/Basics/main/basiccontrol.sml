(* basiccontrol.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
signature BASIC_CONTROL = sig
    (* if false, suppress all warning messages *)
    val printWarnings : bool ref
end

structure BasicControl : BASIC_CONTROL = struct
    val printWarnings = ref true
end
