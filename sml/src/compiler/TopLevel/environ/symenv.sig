(* symenv.sig
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories
 *)

signature SYMENV = sig

    type lambda = Lambda.lexp
    type symenv
    type pid = PersStamps.persstamp

    val empty: symenv
    val look: symenv -> pid -> lambda option
    val bind: pid * lambda * symenv -> symenv
    val atop: symenv * symenv -> symenv
    val remove: pid list * symenv -> symenv
    val consolidate: symenv -> symenv
    val singleton: pid * lambda -> symenv

end

(*
 * $Log: symenv.sig,v $
 * Revision 1.1.1.1  1997/01/14  01:38:37  george
 *   Version 109.24
 *
 *)
