signature GROUPREG = sig
    val clear : unit -> unit
    val register : AbsPath.t * GenericVC.Source.inputSource -> unit
    val lookup : AbsPath.t -> GenericVC.Source.inputSource
    val registered : AbsPath.t -> bool
end

structure GroupReg :> GROUPREG = struct

    val m  =
	ref (AbsPathMap.empty: GenericVC.Source.inputSource AbsPathMap.map)

    fun clear () = m := AbsPathMap.empty
    fun register (g, s) = m := AbsPathMap.insert (!m, g, s)
    fun lookup g = valOf (AbsPathMap.find (!m, g))
    fun registered g = isSome (AbsPathMap.find (!m, g))
end
