(* COPYRIGHT (c) 1996 Bell Laboratories. *)
(* interact.sig *)

signature INTERACT =
sig
  exception Interrupt
  val interact : unit -> unit
  val useFile  : string -> unit
  val useStream : TextIO.instream -> unit
  val initialize : unit -> unit
  val evalStream : TextIO.instream * SCEnv.Env.environment -> 
                       SCEnv.Env.environment

  val installCompManager:
      (Ast.dec *
       { get: unit -> SCEnv.Env.environment,
	 set: SCEnv.Env.environment -> unit } *
       { get: unit -> Environment.environment,
	 set: Environment.environment -> unit }
       -> unit) option
      -> unit

  (* These mUse functions should really be part of the Open Compiler *)
  val mUseFile : (int->bool) -> string -> unit
  val mUseFile_reset : unit -> unit
  val mUseFile_add : (((unit -> unit) * string)list) -> unit
  val mUseFile_list : unit -> (((unit -> unit)*string)list) list

end  (* signature INTERACT *)

