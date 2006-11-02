(* win32-text-io.sml
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 *
 * The implementation of the TextIO stack on Win32 systems.
 *)

structure TextIO = TextIOFn (structure OSPrimIO = Win32TextPrimIO);
