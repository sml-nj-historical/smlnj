(* int-inf.sig
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * This package is derived from Andrzej Filinski's bignum package.
 *
 *)

signature INT_INF =
  sig
    include INTEGER

    val divmod  : (int * int) -> (int * int)
    val quotrem : (int * int) -> (int * int)
    val pow : (int * Int.int) -> int
    val log2 : int -> Int.int

  end (* signature INT_INF *)


(*
 * $Log: int-inf.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:40:04  george
 * Version 110.5
 *
 *)
