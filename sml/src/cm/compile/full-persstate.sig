(*
 * "Full" persistent state.
 *   Includes the "Recomp" state as well as caches for results of
 *   dynamic execution.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
local
    type pid = GenericVC.PersStamps.persstamp
    type env = GenericVC.Environment.dynenv
in
signature FULL_PERSSTATE = sig

    include RECOMP_PERSSTATE

    val exec_look_sml :
	SmlInfo.info * GeneralParams.info * pid option -> env option
    val exec_memo_sml :
	SmlInfo.info * env * SmlInfo.info list * BinInfo.info list -> unit

    val exec_look_stable :
	BinInfo.info * GeneralParams.info * pid option -> env option
    val exec_memo_stable :
	BinInfo.info * env * BinInfo.info list -> unit

    val rememberShared : unit -> unit
end
end
