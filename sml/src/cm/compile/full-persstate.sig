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

    type exec_memo = { dyn: GenericVC.Environment.dynenv, dts: DynTStamp.dts }

    val exec_look_sml : SmlInfo.info * DynTStamp.dts -> exec_memo option
    val exec_memo_sml : SmlInfo.info * exec_memo -> unit

    val exec_look_stable : BinInfo.info * DynTStamp.dts -> exec_memo option
    val exec_memo_stable : BinInfo.info * exec_memo -> unit
end