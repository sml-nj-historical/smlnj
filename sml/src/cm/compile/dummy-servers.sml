(*
 * Handling compile-servers under non-Unix systems.
 *
 *  This is just a placeholder that disables parallel make on non-supported
 *  systems.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure Servers :> SERVERS = struct
    fun start _ = raise Fail "compile server facility not available"
    fun stop _ = false
    fun kill _ = ()
    fun waitforall () = ()
    fun cm _ = ()
    fun cmb _ = ()
    fun compile _ = false
    fun disable () = ()
    fun enable () = ()
end
