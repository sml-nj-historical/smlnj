(* ordkey.sig
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

signature ORD_KEY =
  sig
    type ord_key

    val cmpKey : ord_key * ord_key -> order

  end (* ORD_KEY *)

(*
 * $Log: ordkey.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:14  george
 * Version 110.5
 *
 *)
