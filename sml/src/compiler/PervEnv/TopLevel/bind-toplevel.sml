(* bind-toplevel.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This file binds the top-level identifiers from SML Standard Library in
 * the pervasive top-level environment.
 *
 *)

open General


(* top-level types *)

datatype bool = datatype bool
datatype list = datatype list
datatype ref = datatype PrimTypes.ref

(* top-level value identifiers *)

(* Misc *)
val op =  : ''a * ''a -> bool = InlineT.=
val op <> : ''a * ''a -> bool = InlineT.<>
val vector = Vector.fromList

(* Bool *)
val not = Bool.not

(* Option *)
datatype option = datatype Option.option
exception Option = Option.Option
val getOpt = Option.getOpt
val isSome = Option.isSome
val valOf = Option.valOf

(* Int *)
type int = Int.int

(* Word *)
type word = Word.word

(* Real *)
type real = Real.real
val real = Real.fromInt
val trunc = Real.trunc
val floor = Real.floor
val ceil = Real.ceil
val round = Real.round


(* List *)
exception Empty = List.Empty
val null   = List.null
val hd     = List.hd
val tl     = List.tl
val length = List.length
val rev    = List.rev
val (op @) = List.@
val app    = List.app
val map    = List.map
val foldr  = List.foldr
val foldl  = List.foldl

(* Array *)
type 'a array = 'a Array.array

(* Vector *)
type 'a vector = 'a Vector.vector

(* Char *)
type char = Char.char
val ord = Char.ord
val chr = Char.chr

(* String *)
type string = String.string
val size	= String.size
val str		= String.str
val concat	= String.concat
val implode	= String.implode
val explode	= String.explode
val substring	= String.substring
val (op ^)	= String.^

(* Substring *)
type substring = Substring.substring

(* I/O *)
val print = PrintHook.print


(*
 * $Log$
 *)
