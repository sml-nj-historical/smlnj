signature PRIMITIVE = sig

    type primitive

    val fromString : string -> primitive
    val toString : primitive -> string

    val exports: primitive -> SymbolSet.set
end

structure Primitive :> PRIMITIVE = struct

    type primitive = Dummy.t

    fun fromString s = Dummy.f ()
    fun toString p = Dummy.f ()
    fun exports p = Dummy.f ()
end
