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
 * Revision 1.1.1.1  1998/04/08 18:39:55  george
 * Version 110.5
 *
 *)
