(* text-prim-io-sig.sml
 *
 * COPYRIGHT (c) 2001 Lucent Technologies, Bell labs
 *
 *)
signature TEXT_PRIM_IO = sig

    include PRIM_IO

    val stdIn  : unit -> reader
    val stdOut : unit -> writer
    val stdErr : unit -> writer

    val strReader : string -> reader

end
