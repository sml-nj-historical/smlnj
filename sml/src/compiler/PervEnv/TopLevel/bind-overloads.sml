(* bind-overloads.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This file defines the infix definitions and overloadings in the
 * pervasive top-level environment.
 *
 *)

infix 7  * / mod div
infix 6 ^ + -
infix 3 := o
infix 4 > < >= <= = <>
infixr 5 :: @
infix 0 before

overload ~ :   ('a -> 'a)
  as  Int.~ and Int32.~
  and Real.~
overload + :   ('a * 'a -> 'a)
  as  Int.+  and Int32.+
  and Word8.+ and Word31.+ and Word32.+
  and Real.+ 
overload - :   ('a * 'a -> 'a)
  as  Int.- and Int32.-
  and Word8.- and Word31.- and Word32.-
  and Real.-
overload * :   ('a * 'a -> 'a)
  as  Int.* and Int32.*
  and Word8.* and Word31.* and Word32.*
  and Real.*
overload / : ('a * 'a -> 'a)
  as Real./
overload div : ('a * 'a -> 'a)
  as  Int.div and Int32.div
  and Word8.div and Word31.div and Word32.div
overload mod : ('a * 'a -> 'a)
  as  Int.mod and Int32.mod
  and Word8.mod and Word31.mod and Word32.mod
overload < :   ('a * 'a -> bool)
  as  Int.< and Int32.<
  and Word8.< and Word31.< and Word32.<
  and Real.<
  and Char.<
  and String.<
overload <= :   ('a * 'a -> bool)
  as  Int.<= and Int32.<=
  and Word8.<= and Word31.<= and Word32.<=
  and Real.<=
  and Char.<=
  and String.<=
overload > :   ('a * 'a -> bool)
  as  Int.> and Int32.>
  and Word8.> and Word31.> and Word32.>
  and Real.>
  and Char.>
  and String.>
overload >= :   ('a * 'a -> bool)
  as  Int.>= and Int32.>=
  and Word8.>= and Word31.>= and Word32.>=
  and Real.>=
  and Char.>=
  and String.>=
overload abs : ('a -> 'a)
  as  Int.abs and Int32.abs
  and Real.abs


(*
 * $Log$
 *)
