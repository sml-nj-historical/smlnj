(* pre-bind-largest32.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Bindings of Int, LargeInt, Word, LargeWord and SysWord
 * structures for 32-bit implementations.
 *
 *)

structure Int = Int31
structure Word = Word31
structure LargeInt = Int32
structure LargeWord = Word32
structure Real = Real64
structure LargeReal = Real64
structure SysWord = Word32
structure Position = Int31

(*
 * $Log: pre-bind-largest32.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:40:05  george
 * Version 110.5
 *
 *)
