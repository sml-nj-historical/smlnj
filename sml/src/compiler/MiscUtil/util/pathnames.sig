(* pathnames.sig
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

signature PATHNAMES =
  sig
    val explodePath :string -> string list
    val implodePath :string list -> string
    val trim :string -> string
  end

(*
 * $Log: pathnames.sig,v $
 * Revision 1.1.1.1  1997/01/14  01:38:49  george
 *   Version 109.24
 *
 *)
