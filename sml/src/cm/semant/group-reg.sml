(*
 * The "group registry".  CM uses this to remember which groups it is
 * currently working on and what the corresponding input sources are.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature GROUPREG = sig

    type groupreg

    val new : unit -> groupreg
    val register : groupreg -> SrcPath.t * GenericVC.Source.inputSource -> unit
    val lookup : groupreg -> SrcPath.t -> GenericVC.Source.inputSource
    val registered : groupreg -> SrcPath.t -> bool
    val error :
	groupreg
	-> SrcPath.t * GenericVC.SourceMap.region
	-> GenericVC.ErrorMsg.complainer
end

structure GroupReg :> GROUPREG = struct

    type groupreg = GenericVC.Source.inputSource SrcPathMap.map ref

    fun new () = ref SrcPathMap.empty : groupreg

    fun register gr (p, s) = gr := SrcPathMap.insert (!gr, p, s)
    fun lookup gr p = valOf (SrcPathMap.find (!gr, p))
	handle Option => raise Fail "GroupReg.lookup"
    fun registered gr g = isSome (SrcPathMap.find (!gr, g))
    fun error gr (g, r) = GenericVC.ErrorMsg.error (lookup gr g) r
end
