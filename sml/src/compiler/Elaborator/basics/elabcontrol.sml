(* elabcontrol.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *
 * Flags controlling the elaborator.
 *)
structure ElabControl = struct

    val eedebugging = ref false
    val mudebugging = ref false
    val etdebugging = ref false
    val esdebugging = ref false
    val insdebugging = ref false
    val smdebugging = ref false
    val emdebugging = ref false

    val internals = ref false

    val markabsyn = ref true

    val boxedconstconreps = ref false

    val multDefWarn = ref false
    val shareDefError = ref true
    val valueRestrictionLocalWarn = ref false
    val valueRestrictionTopWarn = ref true
    val instantiateSigs = ref true
end
