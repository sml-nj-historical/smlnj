(*
 * Output of feedback and diagnostics.
 *
 *   Copyright (c) 1999 by Lucent Technologies, Bell Laboratories.
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature SAY = sig
    val say: string -> unit
    val vsay: string -> unit
    val dsay: string -> unit
end

structure Say :> SAY = struct

    structure Print = GenericVC.Control.Print

    fun say s = (Print.say s; Print.flush ())

    fun csay cnd s = if cnd NONE then say s else ()
    val vsay = csay StdConfig.verbose
    val dsay = csay StdConfig.debug
end
