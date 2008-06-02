(* This example potentially produces an invalid deBruijn index TV(0,0)
 * This happens when the APPstr argtycs are given tdepths of 1. 
 * The depth for procStrexp for the definition of functor F should not be
 * incremented. 
 *)
signature S = 
  sig
    type ord_key

    val compare : ord_key * ord_key -> order

  end (* ORD_KEY *)

functor RedBlackMapFnQ (K : S) (* :> ORD_MAP where Key = K *) =
  struct

    (* structure Key = K*)

  end;


functor F(Ord: S) =
struct
  structure M = RedBlackMapFnQ(Ord)
end