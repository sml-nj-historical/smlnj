(* just a placeholder so far *)

(*
 * Configurable path anchors for new CM.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)

signature PATHCONFIG = sig
    val configAnchor : string -> (unit -> string) option
end

structure PathConfig :> PATHCONFIG = struct
    (* this is bogus -- should not be hard-wired like this *)
    fun configAnchor "smlnj" = SOME (fn () => "/home/blume/ML/current/lib")
      | configAnchor _ = NONE
end
