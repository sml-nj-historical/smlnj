(* command-line-sig.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 * extracted from command-line.mldoc (v. 1.1; 1997-09-01)
 *)

signature COMMAND_LINE =
  sig
    val name : unit -> string
    val arguments : unit -> string list
    
  end
