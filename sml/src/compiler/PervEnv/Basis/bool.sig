(* bool.sig
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature BOOL =
  sig

    datatype bool = true | false
    val not : bool -> bool

    val toString   : bool -> string
    val fromString : string -> bool option
    val scan : (char, 'a) StringCvt.reader -> (bool, 'a) StringCvt.reader

  end


(*
 * $Log: bool.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:40:04  george
 * Version 110.5
 *
 *)
