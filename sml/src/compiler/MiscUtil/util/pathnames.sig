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
 * Revision 1.1.1.1  1998/04/08 18:39:16  george
 * Version 110.5
 *
 *)
