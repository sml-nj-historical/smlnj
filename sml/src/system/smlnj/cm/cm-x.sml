(*
 * "minimal" CM
 *   This is a temporary hack because CM' autoloads much faster than
 *   CM for reasons related to how static environments are implemented.
 *   Once the environment problem is fixed, this hack should go away.
 *
 * (C) 2000 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure CM' : sig
    val autoload : string -> bool
    val make : string -> bool
    val recomp : string -> bool
    val stabilize : bool -> string -> bool
    val symval : string -> { get : unit -> int option,
			     set : int option -> unit }
end = CM
