(* pptable.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
structure CompilerPPTable = struct
    val install_pp 
        : string list -> (PrettyPrint.ppstream -> 'a -> unit) -> unit
      = Unsafe.cast PPTable.install_pp
end (* PPTable *)
