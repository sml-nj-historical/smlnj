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

  val installCompManagers:
      { manageImport : Ast.dec * EnvRef.envref -> unit,
	managePrint : Symbol.symbol * EnvRef.envref -> unit,
	getPending : unit -> Symbol.symbol list } -> unit

end  (* signature INTERACT *)
