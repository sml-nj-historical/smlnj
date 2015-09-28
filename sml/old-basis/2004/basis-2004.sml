(* basis-2004.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This file rebinds various basis module names to their 2004 versions.
 *)

(* the 2004 versions of the modified Basis signatures *)
signature ARRAY =
  sig
    type 'a array
    type 'a vector

    val maxLen   : int

    val array    : int * 'a -> 'a array
    val fromList : 'a list -> 'a array
    val tabulate : int * (int -> 'a) -> 'a array

    val length   : 'a array -> int
    val sub      : 'a array * int -> 'a
    val update   : 'a array * int * 'a -> unit

    val vector   : 'a array -> 'a vector

    val copy     : { src : 'a array, dst : 'a array, di : int } -> unit
    val copyVec  : { src : 'a vector, dst : 'a array, di : int } -> unit

    val appi    : (int * 'a -> unit) -> 'a array -> unit
    val app     : ('a -> unit) -> 'a array -> unit
    val modifyi : (int * 'a -> 'a) -> 'a array -> unit
    val modify  : ('a -> 'a) -> 'a array -> unit
    val foldli  : (int * 'a * 'b -> 'b) -> 'b -> 'a array -> 'b
    val foldri  : (int * 'a * 'b -> 'b) -> 'b -> 'a array -> 'b
    val foldl   : ('a * 'b -> 'b) -> 'b -> 'a array -> 'b
    val foldr   : ('a * 'b -> 'b) -> 'b -> 'a array -> 'b

    val findi   : (int * 'a -> bool) -> 'a array -> (int * 'a) option
    val find    : ('a -> bool) -> 'a array -> 'a option
    val exists  : ('a -> bool) -> 'a array -> bool
    val all     : ('a -> bool) -> 'a array -> bool
    val collate : ('a * 'a -> order) -> 'a array * 'a array -> order
  end

signature LIST =
  sig

    datatype 'a list = nil | :: of ('a * 'a list)

    exception Empty

    val null : 'a list -> bool 
    val hd   : 'a list -> 'a                (* raises Empty *)
    val tl   : 'a list -> 'a list           (* raises Empty *)
    val last : 'a list -> 'a                (* raises Empty *)

    val getItem : 'a list -> ('a * 'a list) option

    val nth  : 'a list * int -> 'a       (* raises Subscript *)
    val take : 'a list * int -> 'a list  (* raises Subscript *)
    val drop : 'a list * int -> 'a list  (* raises Subscript *)

    val length : 'a list -> int 

    val rev : 'a list -> 'a list 

    val @         : 'a list * 'a list -> 'a list
    val concat    : 'a list list -> 'a list
    val revAppend : 'a list * 'a list -> 'a list

    val app        : ('a -> unit) -> 'a list -> unit
    val map        : ('a -> 'b) -> 'a list -> 'b list
    val mapPartial : ('a -> 'b option) -> 'a list -> 'b list

    val find      : ('a -> bool) -> 'a list -> 'a option
    val filter    : ('a -> bool) -> 'a list -> 'a list
    val partition : ('a -> bool ) -> 'a list -> ('a list * 'a list)

    val foldr : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b

    val exists : ('a -> bool) -> 'a list -> bool
    val all    : ('a -> bool) -> 'a list -> bool

    val tabulate : (int * (int -> 'a)) -> 'a list   (* raises Size *)

    val collate : ('a * 'a -> order) -> 'a list * 'a list -> order

  end (* signature LIST *)

signature LIST_PAIR =
  sig

    exception UnequalLengths
    val zip    : ('a list * 'b list) -> ('a * 'b) list
    val zipEq  : ('a list * 'b list) -> ('a * 'b) list
    val unzip  : ('a * 'b) list -> ('a list * 'b list)
    val map    : ('a * 'b -> 'c) -> ('a list * 'b list) -> 'c list
    val mapEq  : ('a * 'b -> 'c) -> ('a list * 'b list) -> 'c list
    val app    : ('a * 'b -> unit) -> ('a list * 'b list) -> unit
    val appEq  : ('a * 'b -> unit) -> ('a list * 'b list) -> unit
    val foldl  : (('a * 'b * 'c) -> 'c) -> 'c -> ('a list * 'b list) -> 'c
    val foldr  : (('a * 'b * 'c) -> 'c) -> 'c -> ('a list * 'b list) -> 'c
    val foldlEq: (('a * 'b * 'c) -> 'c) -> 'c -> ('a list * 'b list) -> 'c
    val foldrEq: (('a * 'b * 'c) -> 'c) -> 'c -> ('a list * 'b list) -> 'c
    val all    : ('a * 'b -> bool) -> ('a list * 'b list) -> bool
    val allEq  : ('a * 'b -> bool) -> ('a list * 'b list) -> bool
    val exists : ('a * 'b -> bool) -> ('a list * 'b list) -> bool

  end (* signature LIST_PAIR *)

signature MONO_ARRAY =
  sig

    eqtype array
    type elem
    type vector

    val maxLen : int

  (* array creation functions *)
    val array    : int * elem -> array
    val fromList : elem list -> array
    val tabulate : int * (int -> elem) -> array

    val length   : array -> int
    val sub      : array * int -> elem
    val update   : array * int * elem -> unit

    val vector   : array -> vector
    val copy     : { src : array, dst : array, di : int } -> unit
    val copyVec  : { src : vector, dst : array, di : int } -> unit


    val appi   : (int * elem -> unit) -> array -> unit
    val app    : (elem -> unit) -> array -> unit
    val modifyi: (int * elem -> elem) -> array -> unit
    val modify : (elem -> elem) -> array -> unit

    val foldli : (int * elem * 'a -> 'a) -> 'a -> array -> 'a
    val foldri : (int * elem * 'a -> 'a) -> 'a -> array -> 'a
    val foldl  : (elem * 'a -> 'a) -> 'a -> array -> 'a
    val foldr  : (elem * 'a -> 'a) -> 'a -> array -> 'a

    val findi   : (int * elem -> bool) -> array -> (int * elem) option
    val find    : (elem -> bool) -> array -> elem option
    val exists  : (elem -> bool) -> array -> bool
    val all     : (elem -> bool) -> array -> bool
    val collate : (elem * elem -> order) -> array * array -> order

  end

signature MONO_VECTOR =
  sig

    type vector
    type elem

    val maxLen : int

  (* vector creation functions *)
    val fromList : elem list -> vector
    val tabulate : int * (int -> elem) -> vector

    val length   : vector -> int
    val sub      : vector * int -> elem
    val concat   : vector list -> vector

    val update : vector * int * elem -> vector

    val appi   : (int * elem -> unit) -> vector -> unit
    val app    : (elem -> unit) -> vector -> unit
    val mapi   : (int * elem -> elem) -> vector -> vector
    val map    : (elem -> elem) -> vector -> vector
    val foldli : (int * elem * 'a -> 'a) -> 'a -> vector -> 'a
    val foldri : (int * elem * 'a -> 'a) -> 'a -> vector -> 'a
    val foldl  : (elem * 'a -> 'a) -> 'a -> vector -> 'a
    val foldr  : (elem * 'a -> 'a) -> 'a -> vector -> 'a

    val findi  : (int * elem -> bool) -> vector -> (int * elem) option
    val find   : (elem -> bool) -> vector -> elem option
    val exists : (elem -> bool) -> vector -> bool
    val all    : (elem -> bool) -> vector -> bool
    val collate: (elem * elem -> order) -> vector * vector -> order

  end

signature OPTION =
  sig
    datatype 'a option = NONE | SOME of 'a

    exception Option

    val getOpt         : ('a option * 'a) -> 'a
    val isSome         : 'a option -> bool
    val valOf          : 'a option -> 'a
    val filter         : ('a -> bool) -> 'a -> 'a option
    val join           : 'a option option -> 'a option
    val app            : ('a -> unit) -> 'a option -> unit
    val map            : ('a -> 'b) -> 'a option -> 'b option
    val mapPartial     : ('a -> 'b option) -> 'a option -> 'b option
    val compose        : (('a -> 'b) * ('c -> 'a option)) -> 'c -> 'b option
    val composePartial : (('a -> 'b option) * ('c -> 'a option)) -> 'c -> 'b option

  end

signature STRING =
  sig
    eqtype char
    eqtype string

    val maxSize : int
    val size      : string -> int

    val sub       : string * int -> char

    val str       : char -> string
    val extract   : string * int * int option -> string
    val substring : string * int * int -> string

    val ^         : string * string -> string
    val concat    : string list -> string
    val concatWith : string -> string list -> string

    val implode   : char list -> string
    val explode   : string -> char list
    val map       : (char -> char) -> string -> string
    val translate : (char -> string) -> string -> string
    val tokens    : (char -> bool) -> string -> string list
    val fields    : (char -> bool) -> string -> string list

    val isPrefix    : string -> string -> bool
    val isSubstring : string -> string -> bool
    val isSuffix    : string -> string -> bool

    val compare  : string * string -> order
    val collate  : (char * char -> order) -> string * string -> order

    val <  : (string * string) -> bool
    val <= : (string * string) -> bool
    val >  : (string * string) -> bool
    val >= : (string * string) -> bool

    val toString    : string -> String.string
    val scan        : (char, 'a) StringCvt.reader -> (string, 'a) StringCvt.reader
    val fromString  : String.string -> string option
    val toCString   : string -> String.string
    val fromCString : String.string -> string option

  end

signature TEXT =
  sig
    structure Char            : CHAR
    structure String          : STRING
    structure Substring       : SUBSTRING
    structure CharVector      : MONO_VECTOR
    structure CharArray       : MONO_ARRAY
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
  end

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
