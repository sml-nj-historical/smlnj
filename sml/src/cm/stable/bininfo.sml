signature BININFO = sig

    type info

    val eq : info * info -> bool
    val describe : info -> string
end

structure BinInfo :> BININFO = struct

    type info = Dummy.t

    fun eq (_, _) = true

    fun describe _ = "bininfo"
end
