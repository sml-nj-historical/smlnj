(* command-line.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *)

structure CommandLine : COMMAND_LINE =
  struct
    val name = SMLofNJ.getCmdName
    val arguments = SMLofNJ.getArgs
  end;

(*
 * $Log: command-line.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:40:05  george
 * Version 110.5
 *
 *)

