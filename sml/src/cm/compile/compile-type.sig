signature COMPILATION_TYPE = sig

    type env
    type envdelta

    val layer : env * env -> env
    val filter : envdelta * SymbolSet.set -> env
    val nofilter : envdelta -> env

    val pervasive : Primitive.configuration -> env

    val primitive : Primitive.configuration -> Primitive.primitive -> envdelta

    val lookstable : BinInfo.info -> envdelta option
    val memostable : BinInfo.info * envdelta -> unit
    val dostable : BinInfo.info * env * Primitive.configuration -> envdelta

    val looksml : SmlInfo.info -> envdelta option
    val memosml : SmlInfo.info * envdelta -> unit
    val dosml : SmlInfo.info * env * Primitive.configuration -> envdelta
end
