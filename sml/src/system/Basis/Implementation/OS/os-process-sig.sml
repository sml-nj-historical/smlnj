(* os-process-sig.sml
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *
 * extracted from os-process.mldoc (v. 1.6; 2000-10-31)
 *)

signature OS_PROCESS =
  sig
    type status
    val success : status
    val failure : status
    val isSuccess : status -> bool
    val system : string -> status
    val atExit : (unit -> unit) -> unit
    val exit : status -> 'a
    val terminate : status -> 'a
    val getEnv : string -> string option
    val sleep : Time.time -> unit
    
  end
