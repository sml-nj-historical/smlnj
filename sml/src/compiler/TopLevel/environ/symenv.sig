(* COPYRIGHT (c) 1995 by Bell Laboratories *)
(* symenv.sig *)

signature SYMENV = sig
    type flint = CompBasic.flint
    type symenv
    type pid = PersStamps.persstamp

    val empty: symenv
    val look: symenv -> pid -> flint option
    val bind: pid * flint * symenv -> symenv
    val atop: symenv * symenv -> symenv
    val remove: pid list * symenv -> symenv
    val consolidate: symenv -> symenv
    val singleton: pid * flint -> symenv

end (* signature SYMENV *)

(*
 * $Log: symenv.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:15  george
 * Version 110.5
 *
 *)
