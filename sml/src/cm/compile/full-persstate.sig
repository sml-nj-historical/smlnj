(*
 * "Full" persistent state.
 *   Includes the "Recomp" state as well as caches for results of
 *   dynamic execution.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature FULL_PERSSTATE = sig

    include RECOMP_PERSSTATE

    type env = GenericVC.Environment.dynenv

    val exec_look_sml : SmlInfo.info * GeneralParams.info -> env option
    val exec_memo_sml :
	SmlInfo.info * env * SmlInfo.info list * BinInfo.info list -> unit

    val exec_look_stable : BinInfo.info * GeneralParams.info -> env option
    val exec_memo_stable :
	BinInfo.info * env * BinInfo.info list -> unit

    val rememberShared : GeneralParams.info -> unit
end