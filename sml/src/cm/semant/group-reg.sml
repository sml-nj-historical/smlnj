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
    val register : groupreg -> AbsPath.t * GenericVC.Source.inputSource -> unit
    val lookup : groupreg -> AbsPath.t -> GenericVC.Source.inputSource
    val registered : groupreg -> AbsPath.t -> bool
end

structure GroupReg :> GROUPREG = struct

    type groupreg = GenericVC.Source.inputSource AbsPathMap.map ref

    fun new () = ref AbsPathMap.empty : groupreg

    fun register gr (p, s) = gr := AbsPathMap.insert (!gr, p, s)
    fun lookup gr p = valOf (AbsPathMap.find (!gr, p))
    fun registered gr g = isSome (AbsPathMap.find (!gr, g))
end
