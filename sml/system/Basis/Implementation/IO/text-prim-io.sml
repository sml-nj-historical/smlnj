(* text-prim-io.sml
 *
 * COPYRIGHT (c) 2007 Fellowship of SML/NJ
 *
 *)

structure TextPrimIO = PrimIO (
    structure Vector = CharVector
    structure Array = CharArray
    structure VectorSlice = CharVectorSlice
    structure ArraySlice = CharArraySlice
    val someElem = #"\000"
    type pos = Position.int
    val compare = PositionImp.compare);


