signature MINIMAL_CM = sig
    val autoload : string -> bool
    val make : string -> bool
    val recomp : string -> bool
    val stabilize : bool -> string -> bool
end
