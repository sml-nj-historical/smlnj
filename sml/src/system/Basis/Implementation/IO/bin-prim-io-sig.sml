(* bin-prim-io-sig.sml
 *
 * COPYRIGHT (c) 2001 Lucent Technologies, Bell Labs
 *
 *)
signature BIN_PRIM_IO = sig
    include PRIM_IO

    val openRd : string -> reader
    val openWr : string -> writer
    val openApp : string -> writer
end
