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
 * $Log$
 *)
