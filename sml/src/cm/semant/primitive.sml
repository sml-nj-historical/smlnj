signature PRIMITIVE = sig

    type primitive

    val exports: primitive -> SymbolSet.set
end

structure Primitive = struct

    type primitive = Dummy.t

    fun exports p = Dummy.f ()
end
