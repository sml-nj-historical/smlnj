(* just a placeholder so far *)

signature PATHCONFIG = sig
    val configAnchor : string -> (unit -> string) option
end

structure PathConfig :> PATHCONFIG = struct
    (* this is bogus -- should not be hard-wired like this *)
    fun configAnchor "basis" = SOME (fn () => "/usr/local/smlnj/lib/basis")
      | configAnchor "smlnj" = SOME (fn () => "/usr/local/smlnj/lib")
      | configAnchor _ = NONE
end
