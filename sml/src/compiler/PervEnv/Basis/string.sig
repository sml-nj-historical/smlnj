(* string.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *)

signature STRING =
  sig
    eqtype char
    eqtype string

    val maxSize : int

    val size      : string -> int
    val sub       : (string * int) -> char
    val substring : (string * int * int) -> string
    val extract   : (string * int * int option) -> string
    val concat    : string list -> string
    val ^         : (string * string) -> string
    val str       : char -> string
    val implode   : char list -> string
    val explode   : string -> char list

    val fromString  : String.string -> string option
    val toString    : string -> String.string
    val fromCString : String.string -> string option
    val toCString   : string -> String.string

    val map       : (char -> char) -> string -> string
    val translate : (char -> string) -> string -> string
    val tokens    : (char -> bool) -> string -> string list
    val fields    : (char -> bool) -> string -> string list

    val isPrefix : string -> string -> bool
    val compare  : (string * string) -> order
    val collate  : ((char * char) -> order) -> (string * string) -> order

    val <= : (string * string) -> bool
    val <  : (string * string) -> bool
    val >= : (string * string) -> bool
    val >  : (string * string) -> bool

  end

(*
 * $Log: string.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:40:04  george
 * Version 110.5
 *
 *)
