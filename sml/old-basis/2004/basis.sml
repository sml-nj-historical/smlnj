(* basis-2004.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This file rebinds various basis module names to their 2004 versions.
 *)

(* rebind basis signatures to their 2004 versions *)
signature ARRAY = ARRAY_2004
signature LIST = LIST_2004
signature LIST_PAIR = LIST_PAIR_2004
signature MONO_ARRAY = MONO_ARRAY_2004
signature MONO_VECTOR = MONO_VECTOR_2004
signature OPTION = OPTION_2004
signature STRING = STRING_2004
signature TEXT = TEXT_2004

(* rebind basis structures using 2004 signatures *)
structure Array : ARRAY = Array
structure CharArray : MONO_ARRAY = CharArray
structure CharVector : MONO_VECTOR = CharVector
structure List : LIST = List
structure ListPair : LIST_PAIR = ListPair
structure Option : OPTION = Option
structure Real64Array : MONO_ARRAY = Real64Array
structure Real64Vector : MONO_VECTOR = Real64Vector
structure Text : TEXT = Text
structure Vector : VECTOR = Vector
structure Word8Array : MONO_ARRAY = Word8Array
structure Word8Vector : MONO_VECTOR = Word8Vector

(* the Text modules are extracted from the Text structure *)
structure CharArray : MONO_ARRAY = Text.CharArray
structure CharVector : MONO_VECTOR = Text.CharVector
structure String : STRING = Text.String
