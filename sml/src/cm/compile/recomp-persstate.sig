(*
 * Signature describing the machinery necessary to maintain
 * "recompilation-related" persistent state.
 *   (In essence this is just a bunch of maps to remember binfile contents.)
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature RECOMP_PERSSTATE = sig
    structure MachDepVC : MACHDEP_VC
    type recomp_memo = { bfc: MachDepVC.Binfile.bfContent,
			 ctxt: GenericVC.Environment.staticEnv }

    (* look_sml implicitly removes stale memos *)
    val recomp_look_sml :
	SmlInfo.info * PidSet.set * GeneralParams.info -> recomp_memo option
    val recomp_memo_sml : SmlInfo.info * recomp_memo -> unit

    val recomp_look_stable : BinInfo.info -> recomp_memo option
    val recomp_memo_stable : BinInfo.info * recomp_memo -> unit

    (* The next two functions are there to be used by
     * an enclosing implementation of FULL_PERSSTATE *)
    val bfc_fetch_sml : SmlInfo.info -> MachDepVC.Binfile.bfContent
    val bfc_fetch_stable : BinInfo.info -> MachDepVC.Binfile.bfContent

    (* ... and this one is for stabilize (cache pre-warming) ... *)
    val pid_fetch_sml : SmlInfo.info -> GenericVC.PersStamps.persstamp option
end
