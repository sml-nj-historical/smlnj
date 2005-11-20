(* win32-bin-io.sml
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 * 
 * The implementation of the BinIO stack on Win32 systems.
 *)

structure BinIO = BinIOFn (structure OSPrimIO = Win32BinPrimIO);
