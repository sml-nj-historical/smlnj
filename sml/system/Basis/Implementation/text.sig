(* text.sig
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 *)

signature TEXT_2004 =
  sig
    structure Char            : CHAR
    structure String          : STRING_2004
    structure Substring       : SUBSTRING
    structure CharVector      : MONO_VECTOR_2004
    structure CharArray       : MONO_ARRAY_2004
    structure CharVectorSlice : MONO_VECTOR_SLICE
    structure CharArraySlice  : MONO_ARRAY_SLICE
    sharing type Char.char = String.char = Substring.char
	= CharVector.elem = CharArray.elem
	= CharVectorSlice.elem = CharArraySlice.elem
    sharing type Char.string = String.string = Substring.string
	= CharVector.vector = CharArray.vector
	= CharVectorSlice.vector = CharArraySlice.vector
    sharing type CharArray.array = CharArraySlice.array
    sharing type CharArraySlice.vector_slice = CharVectorSlice.slice
  end;

signature TEXT_2015 =
  sig
    structure Char            : CHAR
    structure String          : STRING_2015
    structure Substring       : SUBSTRING
    structure CharVector      : MONO_VECTOR_2015
    structure CharArray       : MONO_ARRAY_2015
    structure CharVectorSlice : MONO_VECTOR_SLICE
    structure CharArraySlice  : MONO_ARRAY_SLICE
    sharing type Char.char = String.char = Substring.char
	= CharVector.elem = CharArray.elem
	= CharVectorSlice.elem = CharArraySlice.elem
    sharing type Char.string = String.string = Substring.string
	= CharVector.vector = CharArray.vector
	= CharVectorSlice.vector = CharArraySlice.vector
    sharing type CharArray.array = CharArraySlice.array
    sharing type CharArraySlice.vector_slice = CharVectorSlice.slice
  end;

signature TEXT = TEXT_2015

