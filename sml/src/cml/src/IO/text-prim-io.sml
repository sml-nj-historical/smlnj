(* text-prim-io.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure TextPrimIO = PrimIO (
    structure Vector = CharVector
    structure Array = CharArray
    val someElem = #"\000"
    type pos = Position.int
    val compare = Position.compare);

