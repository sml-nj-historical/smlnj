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

    val exec_look_sml :
	SmlInfo.info * bool * GeneralParams.info -> (env * bool) option
    val exec_memo_sml :	SmlInfo.info * env -> unit

    val exec_look_stable :
	BinInfo.info * bool * GeneralParams.info -> (env * bool) option
    val exec_memo_stable : BinInfo.info * env -> unit

    val rememberShared : unit -> unit
end