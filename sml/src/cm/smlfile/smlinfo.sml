signature SMLINFO = sig

    type info

    type fileoffset = AbsPath.t * int
    type stableinfo = { skeleton: Skeleton.decl, binary: fileoffset }

    val new : { sourcepath: AbsPath.t,
	        group: AbsPath.t,
	        history: string option,
		share: bool option,
		stableinfo: stableinfo option }
	-> info

    val exports : info -> SymbolSet.set
    val describe : info -> string
end

structure SmlInfo :> SMLINFO = struct

    type info = Dummy.t

    type fileoffset = AbsPath.t * int
    type stableinfo = { skeleton: Skeleton.decl, binary: fileoffset }

    fun new { sourcepath, group, history, share, stableinfo } = Dummy.v

    fun exports i = Dummy.f ()
    fun describe i = Dummy.f ()
end
