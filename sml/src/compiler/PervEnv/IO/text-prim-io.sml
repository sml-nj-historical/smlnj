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


(*
 * $Log: text-prim-io.sml,v $
 * Revision 1.1.1.1  1997/01/14 01:38:19  george
 *   Version 109.24
 *
 *)
