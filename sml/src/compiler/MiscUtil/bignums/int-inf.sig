(* int-inf-sig.sml
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * This package is derived from Andrzej Filinski's bignum package.  Eventually,
 * it should be moved to the basis.
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
 * $Log: int-inf-sig.sml,v $
# Revision 1.1  1997/01/28  23:20:12  jhr
# Added IntInf:INT_INF to represent literals.
#
 *)
