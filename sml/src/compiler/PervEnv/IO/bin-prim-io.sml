(* bin-prim-io.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure BinPrimIO = PrimIO (
    structure Vector = Word8Vector
    structure Array = Word8Array
    val someElem = (0w0 : Word8.word)
    type pos = Position.int
    val compare = Position.compare);


(*
 * $Log: bin-prim-io.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:55  george
 * Version 110.5
 *
 *)
