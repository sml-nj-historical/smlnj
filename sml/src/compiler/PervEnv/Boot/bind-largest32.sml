(* bind-largest32.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Pre-basis bindings of Int, LargeInt, Word, LargeWord, SysWord,
 * Real and LargeReal structures for 32-bit implementations.
 *
 *)

structure Int = Int31
structure Word = Word31
structure LargeInt = Int32
structure LargeWord = Word32
structure SysWord = Word32
structure Position = Int31

structure Real = Real64
structure Math = Math64
structure LargeReal = Real64


(*
 * $Log$
 *)
