(* COPYRIGHT (c) 1995 by Bell Laboratories *)
(* symenv.sig *)

signature SYMENV = sig
    type flint = FLINT.prog
    type symenv
    type pid = PersStamps.persstamp

    val empty: symenv
    val look: symenv -> pid -> flint option
    val bind: pid * flint * symenv -> symenv
    val atop: symenv * symenv -> symenv
    val remove: pid list * symenv -> symenv
    val consolidate: symenv -> symenv
    val singleton: pid * flint -> symenv
    val listItemsi: symenv -> (pid * flint) list
    val fromListi: (pid * flint) list -> symenv

    val mk : pid option * flint option -> symenv
end (* signature SYMENV *)

