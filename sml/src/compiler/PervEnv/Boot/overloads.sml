(* overloads.sml
 *
 * Copyright (c) 1989 by AT&T Bell Laboratories 
 *
 * Top-level bindings and overloadings.
 *)

type int = Int.int
type real = Real.real
type string = String.string
type substring = Substring.substring

val floor = Real.floor
val size = String.size
val op ^ = String.^
val substring = String.substring
val concat = String.concat

overload ~ :   ('a -> 'a)        as Int.~   and Real.~
overload + :   ('a * 'a -> 'a)
  as Int.+ and LargeInt.+
 and Real.+
 (*and Word32.+ *)
overload - :   ('a * 'a -> 'a)
  as Int.- and LargeInt.-
 and Real.-
 (*and Word32.- *)
overload * :   ('a * 'a -> 'a)
  as Int.* and LargeInt.*
 and Real.*
overload div : ('a * 'a -> 'a)   as Int.div
overload / :   ('a * 'a -> 'a)   as Real./
overload < :   ('a * 'a -> bool)
  as Int.< and LargeInt.<
 and Real.<
 (*and Word32.< *)
 and String.<
overload <= :   ('a * 'a -> bool)
  as Int.<= and LargeInt.<=
 and Real.<=
 (*and Word32.<= *)
 and String.<=
overload > :   ('a * 'a -> bool)
  as Int.> and LargeInt.>
 and Real.>
 (*and Word32.> *)
 and String.>
overload >= :   ('a * 'a -> bool)
  as Int.>= and LargeInt.>=
 and Real.>=
 (*and Word32.>= *)
 and String.>=

(*
 * $Log: overloads.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:40:05  george
 * Version 110.5
 *
 *)
