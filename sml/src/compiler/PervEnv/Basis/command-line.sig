(* command-line.sig
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *)

signature COMMAND_LINE =
  sig

    val name : unit -> string
    val arguments : unit -> string list

  end;

(*
 * $Log: command-line.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:40:05  george
 * Version 110.5
 *
 *)

