(* os-text-prim-io-sig.sml *)
(* signature of PosixTextPrimIO, and parameter signature of TextIOFn *)

signature OS_TEXT_PRIM_IO =
sig
    include OS_PRIM_IO

    val stdIn  : unit -> PrimIO.reader
    val stdOut : unit -> PrimIO.writer
    val stdErr : unit -> PrimIO.writer

    val strReader : string -> PrimIO.reader

end