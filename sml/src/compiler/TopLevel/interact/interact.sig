(* COPYRIGHT (c) 1996 Bell Laboratories. *)
(* interact.sig *)

signature INTERACT =
sig
  exception Interrupt
  val interact : unit -> unit
  val useFile  : string -> unit
  val useStream : TextIO.instream -> unit
  val evalStream : TextIO.instream * Environment.environment -> 
                   Environment.environment

  val installCompManager: (Ast.dec * EnvRef.envref -> unit) -> unit

end  (* signature INTERACT *)
