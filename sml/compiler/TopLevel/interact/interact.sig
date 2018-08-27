(* COPYRIGHT (c) 1996 Bell Laboratories. *)
(* interact.sig *)

signature INTERACT =
sig
  exception Interrupt

  val interact : unit -> unit
  val useFile  : string -> bool (* returns true if okay and false on error *)
  val useStream : TextIO.instream -> unit
  val evalStream : TextIO.instream * Environment.environment -> Environment.environment

  val withErrorHandling : bool -> (* true: treat all exns like usercode exns *)
      { thunk: unit -> unit, flush: unit -> unit, cont: exn -> unit } -> unit

  val installCompManagers:
      { manageImport : Ast.dec * EnvRef.envref -> unit,
	managePrint : Symbol.symbol * EnvRef.envref -> unit,
	getPending : unit -> Symbol.symbol list } -> unit

  val redump_heap_cont : string SMLofNJ.Cont.cont ref

end  (* signature INTERACT *)
