(* just a placeholder so far *)

(*
 * Configurable path anchors for new CM.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)

signature PATHCONFIG = sig
    type mode

    val default : mode

    val configAnchor : mode -> string -> (unit -> string) option
end

(*
 * The names of config anchors must be names of actual files.
 * Function configAnchor will map the name of the anchor to
 * the directory that contains the corresponding file.
 *)
structure PathConfig :> PATHCONFIG = struct
    type mode = Dummy.t

    val default = Dummy.v

    (* this is bogus -- should not be hard-wired like this *)
    fun configAnchor _ "smlnj-lib.cm" =
	SOME (fn () => "/home/blume/ML/current/lib")
      | configAnchor _ _ = NONE
end
