(* text-io.sml
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 * COPYRIGHT (c) 2001 Lucent Technologies, Bell Labs
 *
 * The implementation of the TextIO stack
 *
 *)
structure TextIO :> TEXT_IO
    where type StreamIO.reader = TextPrimIO.reader
    where type StreamIO.writer = TextPrimIO.writer
    where type StreamIO.pos = TextPrimIO.pos
    where type StreamIO.elem = char
   where type StreamIO.vector = string
 = TextIOFn (structure PrimIO = TextPrimIO)
